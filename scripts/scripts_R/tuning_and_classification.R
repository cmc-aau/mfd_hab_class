#!/usr/bin/env Rscript

# Habitat classification with random forest and hyperparameter tuning

## Setting

### Setting seed

set.seed(123)

### Loading libraries

suppressMessages(library(optparse))
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(tidymodels))
suppressMessages(library(themis))
suppressMessages(library(doFuture))
#suppressMessages(library(MESS))
suppressMessages(library(ranger))
suppressMessages(library(tidysdm))
suppressMessages(library(sf))


options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

### Parsing arguments

option_list = list(
  make_option(c("-i", "--input_table"), type="character", 
              help="Table of micobial quantifications (explanatory variables).", metavar="character"),
  make_option(c("-o", "--out_object"), type="character", 
              help="RData object where to save the results.", metavar="character"),
  make_option(c("-m", "--meta"), type="character", 
              help="Minimal metadata file.", metavar="character"),
  make_option(c("-c", "--class"), type="character", 
              help="Desired level of the ontology to model (options:)", metavar="character"),
  make_option(c("-s", "--n_obs"), type="numeric", 
              help="Minimum number of observations of a class to be included in the model.", metavar="numeric"),
  make_option(c("-t", "--threads"), type="numeric", 
              help="Number of cores for parallel processing.", metavar="numeric")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#opt <- list(input_table = "/home/bio.aau.dk/wz65bi/mfd_hab_class/analysis/input_review01/micro_vars/reduced_phylum.csv",
#            meta = "/home/bio.aau.dk/wz65bi/mfd_hab_class/data/2025-02-19_mfd_db.xlsx",
#            class = "Areatype",
#            n_obs = 25,
#            threads = 12)

### Load data

df.input <- data.table::fread(opt$input_table, sep = ",") %>%
  as.data.frame() %>%
  column_to_rownames(var = "V1")

mfd_db <- read_xlsx(opt$meta, sheet = 1)

## Main

### Summarise to class level

mfd_db <- mfd_db %>%
  filter(fieldsample_barcode %in% rownames(df.input),
         coords_reliable != "No",
         !is.na(longitude),
         !is.na(latitude)) %>%
  dplyr::select(fieldsample_barcode,
                mfd_sampletype,
                mfd_areatype,
                mfd_hab1,
                mfd_hab2,
                mfd_hab3,
                latitude,
                longitude) %>%
  mutate(hab_class = case_when(opt$class == "Sampletype" ~ mfd_sampletype,
                               opt$class == "Areatype" ~ paste(mfd_sampletype, mfd_areatype, sep = "; "),
                               opt$class == "MFDO1" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, sep = "; "),
                               opt$class == "MFDO2" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, mfd_hab2, sep = "; "),
                               opt$class == "MFDO3" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, mfd_hab2, mfd_hab3, sep = "; "))) %>%
  dplyr::select(fieldsample_barcode,
                hab_class,
                longitude,
                latitude)

### Geographical thinning

print("Number of samples before spatial thinning (non-reliable and missing coordinates removed):")
print(nrow(mfd_db))
print("Count of the classes before spatial thinning (non-reliable and missing coordinates removed):")
print(table(mfd_db$hab_class))

mfd_db.to_thin <- mfd_db %>%
  st_as_sf(coords = c("longitude", "latitude"))

st_crs(mfd_db.to_thin) <- 4326 # Specify coordinates system

### Define empty data frames to collect results of iterations

for(Iteration in 1:25){
  
  mfd_db.thinned <- mfd_db.to_thin %>%
    group_by(hab_class) %>%
    group_modify(~ thin_by_dist(.x, dist_min = km2m(5))) %>%
    ungroup()
  
  print("Number of samples after spatial thinning (non-reliable and missing coordinates removed):")
  print(nrow(mfd_db.thinned))
  print("Count of the classes after spatial thinning (non-reliable and missing coordinates removed):")
  print(table(mfd_db.thinned$hab_class))
  
  ### Filter classes by observation number
  
  selected_classes <- names(table(mfd_db.thinned$hab_class))[table(mfd_db.thinned$hab_class) >= opt$n_obs]
  selected_classes <- selected_classes[!endsWith(selected_classes, "NA")]
  
  mfd_db.thinned_filtered <- mfd_db %>%
    filter(fieldsample_barcode %in% mfd_db.thinned$fieldsample_barcode,
           hab_class %in% selected_classes)
  
  print(paste0("There are ", length(unique(mfd_db.thinned_filtered$hab_class)), " different classes"))
  print(paste0("The smallest class has ", sort(table(mfd_db.thinned_filtered$hab_class))[1], " different samples"))
  print("Count of the classes:")
  print(table(mfd_db.thinned_filtered$hab_class))
  
  ### Filter samples by selected classes and reorder rows
  
  df.in <- df.input[mfd_db.thinned_filtered$fieldsample_barcode, ]
  
  ### Split train and test set
  
  splits <- df.in %>%
    mutate(hab_class = as.factor(mfd_db.thinned_filtered$hab_class)) %>%
    initial_split(prop = 0.7,
                  strata = hab_class)
  df.training <- training(splits)
  df.testing <- testing(splits)
  
  ### Prepare dataframe
  
  mfd.to_model <- df.training #%>%
    #scale(center = T, scale = T) %>%
    #as.data.frame() %>%
    #mutate(hab_class = as.factor(mfd_db.thinned_filtered$hab_class))
  
  print(dim(mfd.to_model))
  
  ### Define imbalanced classes recipe
  
  imbalanced_classes_rec <- recipe(hab_class~., data = mfd.to_model) %>%
    step_nzv(all_predictors()) %>%
    step_naomit() %>%
    step_smote(hab_class, over_ratio = 0.5) %>%
    step_scale(all_predictors())
  
  ## Grid CV
  cv_fold <- vfold_cv(mfd.to_model, v = 5, strata = "hab_class", repeats = 5)
  
  ### Define F1 micro
  
  f_meas_micro <- function(data, truth, estimate, ...) {
    f_meas(
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      beta = 1,
      estimator = "micro",
      na_rm = TRUE,
      case_weights = NULL#,
      #event_level = yardstick_event_level()
    )
  }
  
  f_meas_micro <- new_class_metric(f_meas_micro, "maximize")
  
  pr_auc_macro_weighted <- function(data, truth, ..., estimate, estimator = "macro_weighted", na_rm = T, event_level, case_weights) {
    pr_auc(data = data,
           truth = {{ truth }},
           (dplyr::starts_with(".pred_") & !{{ estimate }}),
           estimator = {{ estimator }},
           na_rm = {{ na_rm }},
           event_level = {{ event_level }},
           case_weights = {{ case_weights }})
  }
  
  pr_auc_macro_weighted <- new_class_metric(pr_auc_macro_weighted, "maximize")
  
  ### Define metrics to collect
  
  metric.to.compute <- metric_set(kap, pr_auc, pr_auc_macro_weighted, f_meas, f_meas_micro)
  
  ### Function to extract variable importance
  
  get_rand_forest_coefs <- function(x) {
    x %>%
      extract_fit_parsnip() %>%
      vip::vi(method = "model")
  }
  
  ### Define control parameters for the model
  
  ctrl = control_grid(save_pred = T,
                           parallel_over = "everything",
                           extract = get_rand_forest_coefs,
                           verbose = T,
                           save_workflow = TRUE)
  
  ### Define model
  
  rand_forest.spec <- rand_forest(mtry = tune(),
                                  trees = tune(),
                                  min_n = tune()) %>%
    set_engine("ranger",
               importance = "impurity") %>% 
    set_mode("classification")
  
  mfd_hab_models <- workflow_set(preproc = list(default = imbalanced_classes_rec),
                                 models = list(rf = rand_forest.spec),
                                 cross = TRUE) %>%
    option_add(control = ctrl)
  
  ## Tuning
  # set.seed(123)
  
  registerDoFuture()
  plan(multisession, workers = opt$threads)
  
  hab_class.fit <- mfd_hab_models %>%
    workflow_map("tune_grid",
                 resamples = cv_fold,
                 grid = 64,
                 metrics = metric.to.compute,
                 verbose = TRUE)
  
  
  if(!exists("models.collect")){
    models.collect <- (hab_class.fit %>%
                         mutate(Iteration = Iteration))
  } else {
    models.collect <- models.collect %>%
      bind_rows((hab_class.fit %>%
                   mutate(Iteration = Iteration)))
  }
  
  ### Select best model
  
  best.rf.params <- hab_class.fit %>%
    extract_workflow_set_result("default_rf") %>%
    select_best(metric = "kap")
  
  rand_forest.best <- rand_forest(trees = best.rf.params$trees,
                                  min_n = best.rf.params$min_n) %>%
    set_engine("ranger",
               importance = "impurity") %>% 
    set_mode("classification")
  
  rf.wf <- workflow() %>%
    add_recipe(imbalanced_classes_rec) %>%
    add_model(rand_forest.best)
  
  best.rf.fit <- fit(rf.wf, data = df.training)
  
  best.rf.pred <- df.testing %>%
    bind_cols(predict(best.rf.fit, ., type = "prob")) %>%
    mutate(pred_class = (df.testing %>%
                           bind_cols(predict(best.rf.fit, ., type = "class")) %>%
                           pull(.pred_class)))
  
  if(!exists("pred.collect")){
    pred.collect <- (best.rf.pred %>%
                       mutate(Iteration = Iteration))
  } else {
    pred.collect <- pred.collect %>%
      bind_rows((best.rf.pred %>%
                   mutate(Iteration = Iteration)))
  }
  
  rf.conf <- conf_mat(best.rf.pred,
                      truth = hab_class,
                      estimate = pred_class)
  
  rf.conf$table %>%
    as.data.frame() %>%
    rownames_to_column(var = "Predicted") %>%
    ggplot(aes(Prediction, Truth, fill = Freq))+
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low="white", high="#009194") +
    labs(x = "Reference",y = "Prediction") +
    scale_x_discrete(guide = guide_axis(angle = 60))
  
  ### Evaluation
  
  small.set <- metric_set(pr_auc, pr_auc_macro_weighted, roc_auc, kap, f_meas, f_meas_micro)
  
  eval <- best.rf.pred %>%
    dplyr::select(c(dplyr::starts_with(".pred_"), "pred_class", "hab_class")) %>%
    small.set(truth = hab_class,
                      dplyr::starts_with(".pred_"),
                      estimate = pred_class)
  #eval
  
  if(!exists("eval.collect")){
    eval.collect <- (eval %>%
                       mutate(Iteration = Iteration))
  } else {
    eval.collect <- eval.collect %>%
      bind_rows((eval %>%
                   mutate(Iteration = Iteration)))
  }
  
  ### Best Random forest variable importance
  
  rf.imp <- data.frame(Importance = as.numeric(best.rf.fit$fit$fit$fit$variable.importance),
                       Variable = names(best.rf.fit$fit$fit$fit$variable.importance))

  if(!exists("imp.collect")){
    imp.collect <- (rf.imp %>%
                      mutate(Iteration = Iteration))
  } else {
    imp.collect <- imp.collect %>%
      bind_rows((rf.imp %>%
                   mutate(Iteration = Iteration)))
  }
  
}

#taxa.order <- imp.collect %>%
#  group_by(Variable) %>%
#  summarise(imp.mean = mean(Importance)) %>%
#  arrange(desc(imp.mean)) %>%
#  pull(Variable)

#imp.collect %>%
#  filter(Variable %in% taxa.order) %>%
#  mutate(Taxon = factor(Variable, levels = taxa.order)) %>%
#  ggplot(aes(x = Taxon, y = Importance)) +
#  geom_point(stat = "identity") +
#  theme_classic() +
#  scale_x_discrete(guide = guide_axis(angle = 60))

save(models.collect, pred.collect, eval.collect, imp.collect, file = opt$out_object)
