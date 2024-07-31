#!/usr/bin/env Rscript

# Collect the habitat classification models

## Setting

### Setting seed

set.seed(123)

### Loading libraries

suppressMessages(library(optparse))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(tidymodels))
suppressMessages(library(MESS))

options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

### Argument parsing

option_list = list(
  make_option(c("-i", "--input_file"), type="character", 
              help="All models (.RData).", metavar="character"),
  make_option(c("-m", "--metrics_file"), type="character", 
              help="Output metrics file (.csv).", metavar="character"),
  make_option(c("-t", "--classes_auc_file"), type="character", 
              help="Output of per-class AUCs (.csv).", metavar="character"),
  make_option(c("-s", "--classes_aucs_file"), type="character", 
              help="Output of per-class AUCs - both roc and pr (.csv).", metavar="character"),
  make_option(c("-a", "--classes_tpr_file"), type="character",
              help="Output of per-class TPRs (.csv).", metavar="character"),
  make_option(c("-c", "--classes_roc_file"), type="character", 
              help="Output of per-class ROCs (.csv).", metavar="character"),
  make_option(c("-v", "--var_imp_file"), type="character", 
              help="Output of variable importance (.csv).", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

input.files <- (unlist(str_split(opt$input_file, ",")))

print("Parsing the following list of files:")
print(input.files)

## Main

### Set parsing functions

f_auc <- function(df){
  to_screen <- colnames(df)[startsWith(colnames(df), ".pred_")]
  to_return <- roc_curve(data = df, truth = obs, eval(to_screen)) %>%
    group_by(.level) %>%
    summarise(auc.class = auc(1-specificity, sensitivity, from = 0, to = 1, type = "spline", subdivisions = 1000))
  return(to_return)
}

f_roc <- function(df, block){
  to_screen <- colnames(df)[startsWith(colnames(df), ".pred_")]
  to_return <- roc_curve(data = df, truth = obs, eval(to_screen)) %>%
    as.data.frame() %>%
    mutate(block = block) %>%
    slice_sample(n = 100, replace = F) %>%
    as.data.frame()
  return(to_return)
}

f_TPR <- function(x, l){
  l <- unlist(l)
  TP <- sum((l==x)+0)
  TP_FN <- length(l)
  return((TP/TP_FN))
}

f_pr_auc <- function(df){
  
  classes <- unique(df$hab_class)
  
  to_return <- lapply(classes, function(x){
    df %>%
      mutate(hab_class = as.factor((hab_class==x)+0)) %>%
      pr_auc(hab_class, eval(paste0(".pred_", x)), event_level = "second") %>%
      mutate(class = x)
  }) %>%
    bind_rows()
  
  return(to_return)
}

f_roc_auc <- function(df){
  
  classes <- unique(df$hab_class)
  
  to_return <- lapply(classes, function(x){
    df %>%
      mutate(hab_class = as.factor((hab_class==x)+0)) %>%
      roc_auc(hab_class, eval(paste0(".pred_", x)), event_level = "second") %>%
      mutate(class = x)
  }) %>%
    bind_rows()
  
  return(to_return)
}

### Set dataframes

auc.df <- data.frame(.level=c(), m=c(), s=c(), tax_level=c(), hab_level=c())
aucs.df <- data.frame(id=c(), id2=c(), .metric=c(), .estimator=c(), .estimate=c(), hab_class=c(), tax_level=c(), hab_level=c())
roc.df <- data.frame(.level=c(), .threshold=c(), specificity=c(), sensitivity=c(), block=c(), tax_level=c(), hab_level=c())
met.df <- data.frame(.metric=c(), .estimator=c(), mean=c(), n=c(), std_err=c(), .config=c(), tax_level=c(), hab_level=c())
imp.df <- data.frame(Variable=c(), Importance=c(), tax_level=c(), hab_level=c())
tpr.df <- data.frame(id=c(), id2=c(), hab_class=c(), TPR=c(), tax_level=c(), hab_level=c())

### Parse models

for(full_name in input.files){
  
  print(paste0("fullname: ", full_name))
  fname_split <- unlist(str_split(full_name, "/")) %>% tail(n=1)
  print(fname_split)
  fname <- unlist(str_split(fname_split, "\\."))[1]
  tax_level <- unlist(str_split(fname, "_"))[2]
  hab_level <- unlist(str_split(fname, "_"))[3]
  
  print(paste0("fname: ", fname))
  
  load(full_name)
  rand.forest.res <- collect_predictions(rand.forest.fit)
  colnames(rand.forest.res)[(ncol(rand.forest.res)-1)] <- "hab_class"
  rand.forest.met <- collect_metrics(rand.forest.fit)
  
  auc.df.tmp <- rand.forest.res %>%
    mutate(pred = .pred_class,
           obs = hab_class) %>%
    select(-.pred_class, hab_class) %>%
    nest_by(id, id2) %>%
    pull(data) %>% 
    lapply(f_auc) %>%
    bind_rows()
  
  auc.df <- auc.df %>%
    rbind((auc.df.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
  tmp.df <- rand.forest.res %>%
    mutate(pred = .pred_class,
           obs = hab_class) %>%
    mutate(block = paste0(id, "_", id2)) %>%
    select(-.pred_class, hab_class, -id, -id2) %>%
    nest_by(block)
  
  roc.df.tmp <- mapply(f_roc, tmp.df$data, tmp.df$block) %>%
    apply(2, as.data.frame) %>%
    bind_rows()
  
  roc.df <- roc.df %>%
    rbind((roc.df.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
  met.df <- met.df %>%
    rbind((rand.forest.met %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
  imp.tmp <- lapply((1:nrow(rand.forest.fit)),
                    function(x){rand.forest.fit$.extracts[[x]]$.extracts}) %>%
    bind_rows()
  #print(rand.forest.fit$.extracts[[1]]$.extracts)
  
  imp.df <- imp.df %>%
    rbind((imp.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
 
  tpr.tmp <- rand.forest.res %>%
    select(id, id2, hab_class, .pred_class) %>%
    nest_by(id, id2, hab_class) %>%
    summarise(TPR=f_TPR(hab_class, data))

  print(tpr.tmp)

  tpr.df <- tpr.df %>%
    rbind((tpr.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))

  pr_auc.tmp <- rand.forest.res %>%
    nest_by(id, id2) %>%
    mutate(res = list(f_pr_auc(data))) %>%
    unnest(res) %>%
    select(-data)
  
  roc_auc.tmp <- rand.forest.res %>%
    nest_by(id, id2) %>%
    mutate(res = list(f_roc_auc(data))) %>%
    unnest(res) %>%
    select(-data)
  
  aucs.df <- aucs.df %>%
    rbind((pr_auc.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level))) %>%
    rbind((roc_auc.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
}


## Output data

### Write metrics

write_tsv(met.df, file = opt$metrics_file, quote = NULL, col_names = T)

### Write per-class AUCs

write_tsv(auc.df, file = opt$classes_auc_file, quote = NULL, col_names = T)

### Write per-class AUCs

write_tsv(aucs.df, file = opt$classes_aucs_file, quote = NULL, col_names = T)

### Write per-class TPRs

write_tsv(tpr.df, file = opt$classes_tpr_file, quote = NULL, col_names = T)

### Write per-class ROCs

write_tsv(roc.df, file = opt$classes_roc_file, quote = NULL, col_names = T)

### Write variable importance

write_tsv(imp.df, file = opt$var_imp_file, quote = NULL, col_names = T)
