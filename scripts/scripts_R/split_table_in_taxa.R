#!/usr/bin/env Rscript

# Split table into taxonomic ranks

## Setting

### Setting seed

set.seed(123)

### Loading libraries

suppressMessages(library(optparse))
suppressMessages(library(tidyverse))
suppressMessages(library(ampvis2))

options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

### Parsing arguments

option_list = list(
  make_option(c("-i", "--input_table"), type="character", 
              help="Table to rarefy.", metavar="character"),
  make_option(c("-o", "--out_file"), type="character", 
              help="Output prefict for the output file.", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

### Load data

df.input <- read_csv(opt$input_table)


## Main

### Make ampvis object

df.amp <- amp_load(otutable = df.input)

### Aggregate species - not in this analysis

#df.species <- df.amp$abund %>%
#  `rownames<-`(df.amp$tax$Species)

#write.csv(df.species, file = paste0(opt$out_file, "_species.csv"), quote = F, row.names = T, col.names = T)

### Aggregate genera

df.genus <- aggregate_abund(df.amp$abund, df.amp$tax,
                             tax_aggregate = "Genus",
                             format = "abund")

write.csv(df.genus, file = paste0(opt$out_file, "_genus.csv"), quote = F, row.names = T, col.names = T)

rm(df.genus)

### Aggregate families

df.family <- aggregate_abund(df.amp$abund, df.amp$tax,
                             tax_aggregate = "Family",
                             format = "abund")

write.csv(df.family, file = paste0(opt$out_file, "_family.csv"), quote = F, row.names = T, col.names = T)

rm(df.family)

### Aggregate orders

df.order <- aggregate_abund(df.amp$abund, df.amp$tax,
                             tax_aggregate = "Order",
                             format = "abund")

write.csv(df.order, file = paste0(opt$out_file, "_order.csv"), quote = F, row.names = T, col.names = T)

rm(df.order)

### Aggregate classes

df.class <- aggregate_abund(df.amp$abund, df.amp$tax,
                            tax_aggregate = "Class",
                            format = "abund")

write.csv(df.class, file = paste0(opt$out_file, "_class.csv"), quote = F, row.names = T, col.names = T)

rm(df.class)

### Aggregate phyla

df.phylum <- aggregate_abund(df.amp$abund, df.amp$tax,
                            tax_aggregate = "Phylum",
                            format = "abund")

write.csv(df.phylum, file = paste0(opt$out_file, "_phylum.csv"), quote = F, row.names = T, col.names = T)

rm(df.phylum)
