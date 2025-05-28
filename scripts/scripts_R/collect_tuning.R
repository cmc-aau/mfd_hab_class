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
#suppressMessages(library(MESS))

options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

### Argument parsing

option_list = list(
  make_option(c("-i", "--input_file"), type="character", 
              help="All models (.RData).", metavar="character"),
  make_option(c("-m", "--metrics_file"), type="character", 
              help="Output metrics file (.csv).", metavar="character"),
  make_option(c("-a", "--classes_tpr_file"), type="character",
              help="Output of per-class PR AUCs (.csv).", metavar="character"),
  make_option(c("-p", "--classes_prauc_file"), type="character",
              help="Output of per-class TPRs (.csv).", metavar="character"),
  make_option(c("-v", "--var_imp_file"), type="character", 
              help="Output of variable importance (.csv).", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

#load("/home/bio.aau.dk/wz65bi/mfd_hab_class/analysis/output_review02/model_class_Areatype.RData")

input.files <- (unlist(str_split(opt$input_file, ",")))

print("Parsing the following list of files:")
print(input.files)

## Main

### Set parsing functions

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

### Set dataframes

met.df <- data.frame(.metric=c(), .estimator=c(), .estimate=c(), Iteration=c(), tax_level=c(), hab_level=c())
imp.df <- data.frame(Importance=c(), Variable=c(), Iteration=c(), tax_level=c(), hab_level=c())
tpr.df <- data.frame(Iteration=c(), hab_class=c(), TPR=c(), tax_level=c(), hab_level=c())
prauc.df <- data.frame(.metric=c(), .estimator=c(), .estimate=c(), Iteration=c(), tax_level=c(), hab_level=c())

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
  rand.forest.res <- pred.collect
  #colnames(rand.forest.res)[(ncol(rand.forest.res)-1)] <- "hab_class"
  rand.forest.met <- eval.collect
  
  met.df <- met.df %>%
    rbind((rand.forest.met %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
  imp.tmp <- imp.collect
  #print(rand.forest.fit$.extracts[[1]]$.extracts)
  
  imp.df <- imp.df %>%
    rbind((imp.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
 
  tpr.tmp <- rand.forest.res %>%
    select(Iteration, hab_class, pred_class) %>%
    nest_by(Iteration, hab_class) %>%
    summarise(TPR = f_TPR(hab_class, data))

  print(tpr.tmp)

  tpr.df <- tpr.df %>%
    rbind((tpr.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))
  
  prauc.tmp <- f_pr_auc(rand.forest.res)
  
  print(prauc.tmp)
  
  prauc.df <- prauc.df %>%
    rbind((prauc.tmp %>%
             mutate(tax_level = tax_level,
                    hab_level = hab_level)))

}


## Output data

### Write metrics

write_tsv(met.df, file = opt$metrics_file, quote = NULL, col_names = T)

### Write per-class TPRs

write_tsv(tpr.df, file = opt$classes_tpr_file, quote = NULL, col_names = T)

### Write per-class PR AUCs

write_tsv(prauc.df, file = opt$classes_prauc_file, quote = NULL, col_names = T)

### Write variable importance

write_tsv(imp.df, file = opt$var_imp_file, quote = NULL, col_names = T)
