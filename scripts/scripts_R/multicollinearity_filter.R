#!/usr/bin/env Rscript

# Multicollinarity filter for abundance tables

## Setting

### Setting seed

set.seed(123)

### Loading libraries

suppressMessages(library(optparse))
suppressMessages(library(tidyverse))
suppressMessages(library(ampvis2))
suppressMessages(library(usdm))
suppressMessages(library(matrixStats))
suppressMessages(library(parallel))

options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

### Load custom scripts

source(paste0(getwd(), "/scripts/scripts_R/vifcor_blocks.R"))

### Parsing arguments

option_list = list(
  make_option(c("-i", "--input_table"), type="character", 
              help="Table to rarefy.", metavar="character"),
  make_option(c("-o", "--out_file"), type="character", 
              help="Name for the output file.", metavar="character"),
  make_option(c("-s", "--n_samples"), type="numeric", 
              help="Minimum number of obs to retain a sample (x >= threshold).", metavar="numeric"),
  make_option(c("-t", "--threshold"), type="numeric", 
              help="Minimum correlation to be used for multicollinearity screening (x >= threshold).", metavar="numeric"),
  make_option(c("-c", "--cores"), type="numeric", 
              help="Number of cores for parallel processing.", metavar="numeric"),
  make_option(c("-m", "--out_map"), type="character",
              help="Name for the multicollinearity map.", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

df.input <- data.table::fread(opt$input_table, sep = ",") %>%
  as.data.frame() %>%
  column_to_rownames(var = "V1")

## Main

### Filter out unclassified

df.input <- df.input[rownames(df.input)!="Unclassified",]

### Filter out 0-variance taxa

df.input <- df.input[rowSums(df.input)!=0,]

### Ultra-rare taxa filter

df.input <- df.input[apply(X = df.input, MARGIN = 1, FUN = function(x) sum(x != 0)) >= opt$n_samples,]

### Make ampvis object

df.amp <- amp_load(otutable = df.input)

### Transpose table

to.vif <- df.amp$abund %>% t()

### Make syntactically valid names

names2correct <- make.names(colnames(to.vif))
names(names2correct) <- colnames(to.vif)

correct2names <- names(names2correct)
names(correct2names) <- names2correct

### Multicollinearity filter

colnames(to.vif) <- names2correct[colnames(to.vif)]
selected_vars <- vifcor_blockwise(df = to.vif, threads = opt$cores, th = opt$threshold, block_size = 400)

#mfd.v <- vifcor(to.vif, th = opt$threshold)

### Extract non collinear variables

mfd.to_output <- to.vif[, selected_vars]
colnames(mfd.to_output) <- correct2names[colnames(mfd.to_output)]
#mfd.to_output <- exclude(to.vif, mfd.v)

## Output

### Map object

mfd.v <- correct2names[selected_vars]
save(mfd.v, file = opt$out_map)

### Final table

write.csv(mfd.to_output, file = opt$out_file, quote = F, row.names = T, col.names = T)
