#!/usr/bin/env Rscript

# Plot the per-class True Positive Rate (TPR) on the ontology tree

## Setting

BiocManager::install("YuLab-SMU/treedataverse")

### Loading libraries
library(tidyverse)
library(knitr)
library(openxlsx)
library(ggraph)
library(igraph)
library(gtools)
library(treedataverse)
library(dendroextras)
#library(ggvenn)
library(ggtreeExtra)
library(ggnewscale)
#library(ggsankey)
library(wesanderson)
library(ape)
library(gridExtra)
library(ggpubr)

options(width = 500)

## Set the environment
options(stringsAsFactors = F, gsubfn.engine = "R")

Sys.setenv("LANGUAGE"="En")

wd <- "/home/bio.aau.dk/wz65bi/mfd_hab_class"

data.path <- paste0(wd, '/data')
results.path <- paste0(wd, '/analysis/output_review02')
input.path <- paste0(wd, '/analysis/input_review02/micro_vars')


## Load data

mfd_ontology <- read.xlsx(paste0(data.path, "/2025-02-11_mfd-habitat-ontology.xlsx"), sheet = 1)
mfd_db <- read.xlsx(paste0(data.path, "/2025-04-14_mfd_db.xlsx"), sheet = 1)

#auc.df <- data.table::fread(paste0(data.path, "/auc.csv"), sep = "\t")
#aucs.df <- data.table::fread(paste0(data.path, "/aucs.csv"), sep = "\t")
#tpr.df <- data.table::fread(paste0(results.path, "/tpr.tsv"), sep = "\t")

prauc.df <- data.table::fread(paste0(results.path, "/prauc.tsv"), sep = "\t") %>%
  mutate(hab_class = class) %>%
  select(-class)
var_imp <- data.table::fread(paste0(results.path, "/var_imp.tsv"), sep = "\t")
order.df <- data.table::fread(paste0(input.path, "/reduced_genus.csv"), sep = ",")
colnames(order.df)[1] <- "fieldsample_barcode"

# Color

## Palette

## MfD static ontology
dd_mfd1 <- mfd_ontology %>%
  mutate(lvl1 = if_else(is.na(mfd_sampletype), NA, mfd_sampletype),
         lvl2 = if_else(is.na(mfd_areatype), NA, paste0(mfd_sampletype, "; ", mfd_areatype)),
         lvl3 = if_else(is.na(mfd_hab1_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab1_code)),
         lvl4 = if_else(is.na(mfd_hab2_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab2_code)),
         lvl5 = if_else(is.na(mfd_hab3_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab3_code)))

dd_mfd <- mfd_ontology %>%
  mutate(lvl1 = if_else(is.na(mfd_sampletype), NA, mfd_sampletype),
         lvl2 = if_else(is.na(mfd_areatype), NA, paste0(mfd_sampletype, "; ", mfd_areatype)),
         lvl3 = if_else(is.na(mfd_hab1_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab1_code)),
         lvl4 = if_else(is.na(mfd_hab2_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab2_code)),
         lvl5 = if_else(is.na(mfd_hab3_code), NA, paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab3_code)))


mfd_recast <- rbind((data.frame(Code=dd_mfd$lvl1,
                                Label=dd_mfd$lvl1,
                                Natura2000=NA,
                                EUNIS=NA,
                                EMPO="Free-living",
                                mfd_lvl = 1) %>%
                       distinct()),
                    (data.frame(Code=dd_mfd$lvl2,
                                Label=dd_mfd$lvl2,
                                Natura2000=NA,
                                EUNIS=NA,
                                EMPO="Free-living",
                                mfd_lvl = 2) %>%
                       distinct()),
                    (data.frame(Code=dd_mfd$lvl3[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                Label=paste0(dd_mfd$mfd_sampletype[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)], "; ", 
                                             dd_mfd$mfd_areatype[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_hab1[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)]),
                                Natura2000=dd_mfd$Natura2000[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                EUNIS=dd_mfd$EUNIS[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                EMPO=dd_mfd$EMPO[!is.na(dd_mfd$mfd_hab1)&is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                mfd_lvl = 3) %>%
                       distinct()),
                    (data.frame(Code=dd_mfd$lvl4[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                Label=paste0(dd_mfd$mfd_sampletype[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_areatype[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_hab1[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],  "; ",
                                             dd_mfd$mfd_hab2[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)]),
                                Natura2000=dd_mfd$Natura2000[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                EUNIS=dd_mfd$EUNIS[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                EMPO=dd_mfd$EMPO[!is.na(dd_mfd$mfd_hab2)&is.na(dd_mfd$mfd_hab3)],
                                mfd_lvl = 4) %>%
                       distinct()),
                    (data.frame(Code=dd_mfd$lvl5[!is.na(dd_mfd$mfd_hab3)],
                                Label=paste0(dd_mfd$mfd_sampletype[!is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_areatype[!is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_hab1[!is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_hab2[!is.na(dd_mfd$mfd_hab3)], "; ",
                                             dd_mfd$mfd_hab3[!is.na(dd_mfd$mfd_hab3)]), 
                                Natura2000=dd_mfd$Natura2000[!is.na(dd_mfd$mfd_hab3)],
                                EUNIS=dd_mfd$EUNIS[!is.na(dd_mfd$mfd_hab3)],
                                EMPO=dd_mfd$EMPO[!is.na(dd_mfd$mfd_hab3)],
                                mfd_lvl = 5) %>%
                       distinct()))

edges_0_1 <- dd_mfd1 %>%
  mutate(lvl0 = "root") %>%
  select(lvl0, lvl1) %>%
  filter(!is.na(lvl1)) %>%
  unique() %>%
  rename(from=lvl0, to=lvl1)

edges_1_2 <- dd_mfd1 %>%
  select(lvl1, lvl2) %>%
  filter(lvl1!=lvl2 & !is.na(lvl1) & !is.na(lvl2)) %>%
  unique() %>%
  rename(from=lvl1, to=lvl2)
  
edges_2_3 <- dd_mfd1 %>%
  select(lvl2, lvl3) %>%
  filter(lvl2!=lvl3 & !is.na(lvl2) & !is.na(lvl3)) %>%
  unique() %>%
  rename(from=lvl2, to=lvl3)

edges_3_4 <- dd_mfd1 %>%
  select(lvl3, lvl4) %>%
  filter(lvl3!=lvl4 & !is.na(lvl3) & !is.na(lvl4)) %>%
  unique() %>%
  rename(from=lvl3, to=lvl4)

edges_4_5 <- dd_mfd1 %>%
  select(lvl4, lvl5) %>%
  filter(lvl4!=lvl5 & !is.na(lvl4) & !is.na(lvl5)) %>%
  unique() %>%
  rename(from=lvl4, to=lvl5)

edge_list <- rbind(edges_0_1,
                   edges_1_2,
                   edges_2_3,
                   edges_3_4,
                   edges_4_5) %>%
  as.data.frame() %>%
  filter(to!="") %>%
  arrange(from)

graph_Data <- graph_from_data_frame(edge_list %>%
                                       arrange(from) %>%
                                      left_join((mfd_recast %>%
                                                   select(Code, Label)),
                                                by = c("to" = "Code")) %>%
                                      filter(Label%in%prauc.df$hab_class | from == "root") %>%
                                      select(from, to) %>%
                                      distinct() %>%
                                      left_join((mfd_recast %>%
                                                   select(Code, Label)),
                                                by = c("from" = "Code")) %>%
                                      filter(Label%in%prauc.df$hab_class | from == "root") %>%
                                      select(from, to) %>%
                                      distinct())

mfd.tree <- as.phylo(graph_Data) %>%
  left_join((mfd_recast %>%
               filter(Label %in% unique(prauc.df$hab_class)) %>%
  rowwise() %>%
  mutate(metric = runif(1, min=0, max=1))),
            by = c("label" = "Code")) %>%
  mutate(Label = if_else(label == "root", "Root", Label)) %>%
  left_join((prauc.df %>%
               filter(tax_level=="genus") %>%
               group_by(hab_class) %>%
               summarise(PRAUC.mean = mean(.estimate, na.rm = T),
                         PRAUC.sd = sd(.estimate))),
            by = c("Label" = "hab_class"))

p <- mfd.tree %>%
  ggtree(ladderize = T) +
  geom_tiplab(aes(label = Label), align=F, linesize=.5, as_ylab = T) +
  geom_label(aes(label = format(round(PRAUC.mean, 2), 2), fill = PRAUC.mean), size = 2.5) +
  scale_fill_gradientn( colors = c("darkred", "goldenrod1", "yellow2"))

p$data[, "x"] <- p$data[, "mfd_lvl"]+1
p$data[, "x"][is.na(p$data[, "x"])] <- 1

ggsave(p, filename = paste0(results.path, "/class_prauc_tree.png"), width = 10, height = 10)
ggsave(p, filename = paste0(results.path, "/class_prauc_tree.svg"), width = 10, height = 10)


## Variable importance
cutoff <- (var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO1") %>%
  group_by(Variable) %>%
  summarise(Importance.median = median(Importance)) %>%
  ungroup() %>%
  arrange(desc(Importance.median)) %>%
  pull(Importance.median))[20]

var_imp_MFDO1.df <- var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO1") %>%
  group_by(Variable) %>%
  mutate(Importance.median = median(Importance)) %>%
  ungroup() %>%
  filter(Importance.median >= cutoff) %>%
  mutate(Variable = fct_reorder(Variable, desc(Importance.median)))
  
var_imp_MFDO1.plot <- var_imp_MFDO1.df %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(width = 0.8,
               alpha = 0.6,
               fill = NA,
               outlier.size = -1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

cutoff <- (var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO2") %>%
  group_by(Variable) %>%
  summarise(Importance.median = median(Importance)) %>%
  ungroup() %>%
  arrange(desc(Importance.median)) %>%
  pull(Importance.median))[20]

var_imp_MFDO2.df <- var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO2") %>%
  group_by(Variable) %>%
  mutate(Importance.median = median(Importance)) %>%
  ungroup() %>%
  filter(Importance.median >= cutoff) %>%
  mutate(Variable = fct_reorder(Variable, desc(Importance.median)))

var_imp_MFDO2.plot <- var_imp_MFDO2.df %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(width = 0.8,
               alpha = 0.6,
               fill = NA,
               outlier.size = -1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

cutoff <- (var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO3") %>%
  group_by(Variable) %>%
  summarise(Importance.median = median(Importance)) %>%
  ungroup() %>%
  arrange(desc(Importance.median)) %>%
  pull(Importance.median))[20]

var_imp_MFDO3.df <- var_imp %>%
  filter(tax_level=="genus", hab_level=="MFDO3") %>%
  group_by(Variable) %>%
  mutate(Importance.median = median(Importance)) %>%
  ungroup() %>%
  filter(Importance.median >= cutoff) %>%
  mutate(Variable = fct_reorder(Variable, desc(Importance.median)))

var_imp_MFDO3.plot <- var_imp_MFDO3.df %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(width = 0.8,
               alpha = 0.6,
               fill = NA,
               outlier.size = -1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


var_imp_MFDO1.plot
ggsave(var_imp_MFDO1.plot, filename = paste0(results.path, "/var_imp_MFDO1.png"), width = 8, height = 4)
ggsave(var_imp_MFDO1.plot, filename = paste0(results.path, "/var_imp_MFDO1.svg"), width = 8, height = 4)
var_imp_MFDO2.plot
ggsave(var_imp_MFDO2.plot, filename = paste0(results.path, "/var_imp_MFDO2.png"), width = 8, height = 4)
ggsave(var_imp_MFDO2.plot, filename = paste0(results.path, "/var_imp_MFDO2.svg"), width = 8, height = 4)
var_imp_MFDO3.plot
ggsave(var_imp_MFDO3.plot, filename = paste0(results.path, "/var_imp_MFDO3.png"), width = 8, height = 4)
ggsave(var_imp_MFDO3.plot, filename = paste0(results.path, "/var_imp_MFDO3.svg"), width = 8, height = 4)


## Heatmap

order_terminus_heatmap.plot <- mfd_db %>%
  mutate(Areatype=paste0(mfd_sampletype, "; ", mfd_areatype),
         MFDO1=paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab1),
         MFDO2=paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab1, "; ", mfd_hab2),
         MFDO3=paste0(mfd_sampletype, "; ", mfd_areatype, "; ", mfd_hab1, "; ", mfd_hab2, "; ", mfd_hab3)) %>%
  select(fieldsample_barcode, Areatype, MFDO1, MFDO2, MFDO3) %>%
  #mutate(MFDO_terminus = if_else(endsWith(MFDO3, "NA"), MFDO2, MFDO3)) %>%
  mutate(MFDO_terminus = NA,
         MFDO_terminus = if_else(MFDO3%in%p$data$Label[p$data$isTip], MFDO3, MFDO_terminus),
         MFDO_terminus = if_else(MFDO2%in%p$data$Label[p$data$isTip], MFDO2, MFDO_terminus),
         MFDO_terminus = if_else(MFDO1%in%p$data$Label[p$data$isTip], MFDO1, MFDO_terminus),
         MFDO_terminus = if_else(Areatype%in%p$data$Label[p$data$isTip], Areatype, MFDO_terminus)) %>%
  select(fieldsample_barcode, MFDO_terminus) %>%
  filter(MFDO_terminus %in% p$data$Label[p$data$isTip]) %>%
  inner_join((as.data.frame(order.df)[, c("fieldsample_barcode", levels(var_imp_MFDO3.df$Variable))]),
             by = "fieldsample_barcode") %>%
  pivot_longer(names_to = "taxa", values_to = "value", -c(fieldsample_barcode, MFDO_terminus)) %>%
  group_by(MFDO_terminus, taxa) %>%
  summarise(value = median(value)) %>%
  ggplot(aes(x=factor(taxa,
                      levels = levels(var_imp_MFDO3.df$Variable)),
             y=factor(MFDO_terminus,
                      levels = (p$data %>% arrange(y) %>% pull(Label))),
             fill=value)) +
  geom_tile(lwd = .25,
            linetype = 1,
            color = "black") +
  #scale_fill_viridis_c() +
  scale_fill_viridis_c(trans = "sqrt",
                       breaks = c(0, 1, 3, 7),
                       labels = c("0", "1", "3", "7")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

order_terminus_heatmap.plot
ggsave(order_terminus_heatmap.plot, filename = paste0(results.path, "/order_terminus_heatmap.png"), width = 12, height = 12)
ggsave(order_terminus_heatmap.plot, filename = paste0(results.path, "/order_terminus_heatmap.svg"), width = 12, height = 12)
