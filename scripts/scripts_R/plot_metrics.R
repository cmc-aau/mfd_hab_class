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
              help="Table of metrics (.csv).", metavar="character"),
  make_option(c("-o", "--out_plot"), type="character", 
              help="Output plot prefix (it will be saved automatically as .png and .svg).", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

input.df <- data.table::fread(opt$input_file, sep = "\t")
#input.df <- data.table::fread("/home/bio.aau.dk/wz65bi/mfd_hab_class/analysis/output_review02/metrics.tsv", sep = "\t")

## Main

### Plot metrics

to.plot <- input.df %>%
  group_by(.metric, .estimator, hab_level, tax_level) %>%
  summarise(mean = mean(.estimate),
            std_dev = sd(.estimate)) %>%
  filter(.metric %in% c("f_meas", "kap", "pr_auc")) %>%
  mutate(.metric = case_when(.metric == "f_meas" ~ "F1",
                             .metric == "kap" ~ "Kappa",
                             .metric == "pr_auc" ~ "PR AUC")) %>%
  mutate(.metric = paste0(.metric, " (", .estimator, ")")) %>%
  #mutate(margin = qt(0.975, df=25-1) * std_dev / sqrt(25)) %>%
  mutate(hab_level = factor(hab_level, levels = c("Sampletype", "Areatype", "MFDO1", "MFDO2", "MFDO3")),
         tax_level = factor(tax_level, levels = c("phylum", "class", "order", "family", "genus"))) %>%
  ggplot(aes(x = hab_level, y = mean, color = tax_level, fill = tax_level, group = tax_level)) +
  #geom_ribbon(aes(ymin = mean-margin, ymax = mean+margin), alpha =.4, show.legend = F, color = NA) +
  geom_errorbar(aes(ymin = mean-std_dev, ymax = mean+std_dev), width=.2) +
  geom_line() +
  geom_point() +
  facet_wrap(.metric~., nrow = 1) +
  scale_color_viridis_d() +
  theme_classic() +
  ylim(0, 1) +
  theme(panel.grid.major.y = element_line()) +
  labs(x = "Habitat ontology levels", y = "Value of the metric") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        legend.position = "right",
        aspect.ratio = 1)

## Output data

### Save plot

ggsave(to.plot, filename = paste0(opt$out_plot, ".png"), width = 10, height = 3, dpi = "retina")
ggsave(to.plot, filename = paste0(opt$out_plot, ".svg"), width = 10, height = 3, dpi = "retina")


