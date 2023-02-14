#Script for analysis for Figure 5C
# Virus load analysis of testes and ovaries compared to bodies

#Aims of this script:

# 1) Plot the virus loads (as determined by virus RNA relative to RpL-32)
# 2) perform a statistical analysis to see if certain sexes have higher virus load

#Date: 2023-1-19
#Added geom_point over barplot for reviewers

library(tidyverse)
library(readxl)
library(ggpubr)

#set wd to where the file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import the data
df_load <- read_excel("galbut_virus_load_by_sex_strain.xlsx")

#statistical analysis

#does the data follow normality assumptions
#do testing to know wilcoxon vs t test

#doesn't appear to be normalized, do wilcoxon
ggplot(df_load, aes(x = normalized)) + 
  geom_histogram() +
  facet_wrap(~strain)
library(car)
qqp(df_load$normalized)
ggdensity(df_load$normalized)

# rename 25192 -> DGRP-399 
#        25197 -> DGRP-517 
df_load$strain <- as.character(df_load$strain)
df_load <- df_load %>% mutate(strain, strain = if_else(strain == "25192", "DGRP 399", "DGRP 517"))

#Plot each type by the respective sex
#do wilcoxon test
#showing it on a plot to be able to use tidyverse (ggplot2 with ggpubr)

?geom_boxplot

ggplot(df_load, aes(sex, normalized)) +
  geom_boxplot(aes(fill = factor(strain)), alpha = 0.8, outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.15), aes(fill = strain),
             pch=21) +
  labs(y = "Galbut virus RNA levels relative to RpL32",
       x = "") +
  scale_fill_manual(values=c("#009E73", "#CC79A7")) +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank() ) +
  scale_y_continuous(limits=c(0,260)) +
  facet_wrap(~factor(strain)) +
  stat_compare_means(label.x = 0.8, size=3) #this naturally chose to do Wilcoxon

ggsave("fig1_strain_virus_load_v4.pdf", width = 7.5, height = 4, units = "in")
ggsave("fig1_strain_virus_load_v4.tiff", width = 7.5, height = 4, units = "in", dpi=300)



