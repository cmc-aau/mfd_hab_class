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
              help="Table losting ROCs per habitats and taxa (.RData).", metavar="character"),
  make_option(c("-o", "--out_plot"), type="character", 
              help="Output plot prefix (it will be saved automatically as .png and .svg).", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

input.df <- data.table::fread(opt$input_file, sep = "\t")

## Main

### Plot ROCs

to.plot <- input.df %>%
  ggplot(aes(x = (1-as.numeric(specificity)),
             y = as.numeric(sensitivity),
             group = interaction(block, tax_level),
             color = factor(tax_level, levels = c("phylum", "class", "order", "family", "genus")))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlim(0, 1) +
  ylim(0, 1) +
  geom_line(alpha = .4) +
  facet_wrap(.level~., ncol = 10) +
  theme_classic() +
  labs(x = "1 - specificity", y = "sensitivity", color = "Tax. level")
  theme(aspect.ratio = 1)


## Output data

### Save plot

ggsave(to.plot, filename = paste0(opt$out_plot, ".png"), width = 12, height = 10, dpi = "retina")
ggsave(to.plot, filename = paste0(opt$out_plot, ".svg"), width = 12, height = 10, dpi = "retina")


