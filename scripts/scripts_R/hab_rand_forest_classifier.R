#!/usr/bin/env Rscript

# Habitat classification with random forest

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
suppressMessages(library(MESS))

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

### Load data

df.input <- data.table::fread(opt$input_table, sep = ",") %>%
  as.data.frame() %>%
  column_to_rownames(var = "V1")

mfd_db <- read_xlsx(opt$meta)

## Main

### Summarise to class level

mfd_db <- mfd_db %>%
  filter(fieldsample_barcode %in% rownames(df.input)) %>%
  select(fieldsample_barcode,
         mfd_sampletype,
         mfd_areatype,
         mfd_hab1,
         mfd_hab2,
         mfd_hab3) %>%
  mutate(hab_class = case_when(opt$class == "Sampletype" ~ mfd_sampletype,
                                 opt$class == "Areatype" ~ paste(mfd_sampletype, mfd_areatype, sep = "; "),
                                 opt$class == "MFDO1" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, sep = "; "),
                                 opt$class == "MFDO2" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, mfd_hab2, sep = "; "),
                                 opt$class == "MFDO3" ~ paste(mfd_sampletype, mfd_areatype, mfd_hab1, mfd_hab2, mfd_hab3, sep = "; "))) %>%
  select(fieldsample_barcode,
         hab_class)

### Filter classes by observation number

selected_classes <- names(table(mfd_db$hab_class))[table(mfd_db$hab_class) >= opt$n_obs]
selected_classes <- selected_classes[!endsWith(selected_classes, "NA")]

mfd_db <- mfd_db %>%
  filter(hab_class %in% selected_classes)

print(paste0("There are ", length(unique(mfd_db$hab_class)), " different classes"))
print(paste0("The smallest class has ", sort(table(mfd_db$hab_class))[1], " different samples"))
print("Count of the classes:")
print(table(mfd_db$hab_class))

### Filter samples by selected classes and reorder rows

df.input <- df.input[mfd_db$fieldsample_barcode, ]

### Prepare dataframe

mfd.to_model <- df.input %>%
  scale(center = T, scale = T) %>%
  as.data.frame() %>%
  mutate(hab_class = as.factor(mfd_db$hab_class))

print(dim(mfd.to_model))

### Define imbalanced classes recipe

imbalanced_classes_rec <- recipe(hab_class~., data = mfd.to_model) %>%
  step_downsample(hab_class)

### Define linear random forest model

rand_forest.spec <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

### Define workflow

rand.forest.wf <- workflow() %>%
  add_model(rand_forest.spec)  %>%
  add_recipe(imbalanced_classes_rec)

print(rand.forest.wf)

### Define cross validation

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
    case_weights = NULL,
    event_level = yardstick_event_level()
  )
}

f_meas_micro <- new_class_metric(f_meas_micro, "maximize")

### Define metrics to collect

metric.to.compute <- metric_set(kap, mcc, accuracy, precision, recall, sensitivity, roc_auc, pr_auc, f_meas, f_meas_micro)

### Function to extract variable importance

get_rand_forest_coefs <- function(x) {
  x %>%
    extract_fit_parsnip() %>%
    vip::vi()
}

### Define control parameters for the model

ctrl = control_resamples(save_pred = T,
                         parallel_over = "everything",
			 extract = get_rand_forest_coefs,
                         verbose = T)

### Set multi-thread session

registerDoFuture()
plan(multisession, workers = opt$threads)

### Train model

rand.forest.fit <- fit_resamples(rand.forest.wf, resamples = cv_fold, metrics = metric.to.compute, control = ctrl)
print(rand.forest.fit)
## Output

### Save trained model object

save(rand.forest.fit, file = opt$out_object)

