#Microbiome analysis in galbut virus infected and uninfected DGRP flies

#Created by: Shaun Cross
#Date: 7-24-2020

#Question: Does galbut virus infection change the abundance of bacterial microbiome constituents?

library(tidyverse)
library(readxl)
library(ggpubr)
library(ggthemes)
library(Hmisc)


#set wd to where the file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import the data
df_micro <- read_excel("microbiome_constituents_loads.xlsx")

#change stock numbers to names
df_micro$strain <- str_replace(df_micro$strain, "25192", "DGRP-399")
df_micro$strain <- str_replace(df_micro$strain, "25197", "DGRP-517")

#make a plot and use ggpubr to run stats

p_micro <- ggplot(df_micro, aes(x=microbe, y=normalized_load)) +
  geom_boxplot(aes(fill=galbut)) +
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  facet_grid(sex~strain) +
  theme_bw() +
  labs(title = "Relative Microbial Levels in Adult Flies",
       y = "Microbe loads relative to dfd",
       x = "Microbe") +
  scale_y_log10() +
  stat_compare_means(aes(group = galbut),
                     label = "p.format")
p_micro


p_micro_dot <- ggplot(df_micro, aes(x=microbe, y=normalized_load)) +
  geom_dotplot(aes(fill=galbut),
               binaxis = "y",
               binwidth = 0.1,
               stackdir = "center",
               position=position_dodge(0.8),
               dotsize = 2) +
  scale_fill_manual(values=c("#56B4E9", "#D55E00")) +
  facet_grid(sex~strain) +
  theme_bw() +
  labs(title = "Relative Microbial Levels in Adult Flies",
       y = "Microbe loads relative to dfd",
       x = "Microbe") +
  scale_y_log10() +
  stat_summary(fun = median, 
               geom = "crossbar",
               width = 0.8,
               position = position_dodge(0.8),
               aes(group = interaction(microbe, galbut))) +
  stat_compare_means(aes(group = galbut),
                     label = "p.format")

p_micro_dot

ggsave("microbiome_loads_v2.pdf", width = 7.5, height = 8.75, units = "in")


#This chose wilcoxon test as most appropriate method. Double check stats

#DGRP 399
df_micro_92 <- df_micro %>% filter(strain == "DGRP-399")

#wilcoxon test for each microbe
with(df_micro_92, shapiro.test(normalized_load[microbe == "acetobacter"])) #p=0.031
with(df_micro_92, shapiro.test(normalized_load[microbe == "brevis"])) #p=5.09e-5
with(df_micro_92, shapiro.test(normalized_load[microbe == "planatarum"])) #p=0.00012
with(df_micro_92, shapiro.test(normalized_load[microbe == "corynebacteria"])) #p=0.0001003
with(df_micro_92, shapiro.test(normalized_load[microbe == "saccharomyces"])) #p=0.018

#DGRP 517
df_micro_97 <- df_micro %>% filter(strain == "DGRP-517")
#wilcoxon test for each microbe
with(df_micro_97, shapiro.test(normalized_load[microbe == "acetobacter"])) #p=0.0026
with(df_micro_97, shapiro.test(normalized_load[microbe == "brevis"])) #p=00044
with(df_micro_97, shapiro.test(normalized_load[microbe == "planatarum"])) #p=2.51e-5
with(df_micro_97, shapiro.test(normalized_load[microbe == "corynebacteria"])) #p=0.0005277
with(df_micro_97, shapiro.test(normalized_load[microbe == "saccharomyces"])) #p=0.005755

