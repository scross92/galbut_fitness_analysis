#development data analysis

#this script is meant to analyze time to pupation and time to adulthood for infected and uninfected DGRP 399 and 517 flies

#Created by: Shaun Cross
#date: 07-15-2020

library(tidyverse)
library(readxl)
library(ggpubr)

#set wd to where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#First looking at pupation rate data

#read in the dataframe

df_pupae <- read_excel("development_data_pupation.xlsx", sheet = "plot_data_combined")

#change strain # to DGRP name
df_pupae$strain <- str_replace(df_pupae$strain, "25192", "DGRP-399")
df_pupae$strain <- str_replace(df_pupae$strain, "25197", "DGRP-517")

#do normality testing to know wilcoxon vs t test
#doesn't appear to be normalized, do wilcoxon
ggplot(df_pupae, aes(x = hours)) + 
  geom_histogram(bins = 15) +
  facet_wrap(~strain)
#filter for downstream stats and for testing normality
df_pupae_92 <- df_pupae %>% filter(strain == "DGRP-399")
df_pupae_97 <- df_pupae %>% filter(strain == "DGRP-517")

library(car)
qqp(df_pupae_92$hours)
qqp(df_pupae_97$hours)

ggdensity(df_pupae_92$hours)
ggdensity(df_pupae_97$hours)

#25192 shapiro wilk normality test
#both failed (p < 0.001 inferring not normally distributed)
# Shapiro-Wilk normality test for infected development time
with(df_pupae_92, shapiro.test(hours[galbut == "TRUE"]))# p = 1.125 e-15
# Shapiro-Wilk normality test for uninfected development time
with(df_pupae_92, shapiro.test(hours[galbut == "FALSE"])) # p = 6.164e-15

#25197 shapiro wilk normality test
#both failed (p < 0.001 inferring not normally distributed)
# Shapiro-Wilk normality test for infected development time
with(df_pupae_97, shapiro.test(hours[galbut == "TRUE"]))# p = 4.266e-16
# Shapiro-Wilk normality test for uninfected development time
with(df_pupae_97, shapiro.test(hours[galbut == "FALSE"])) # p = 1.121e-10


#check wilcoxon assumptions
gghistogram(df_pupae_92, x = "hours", y = "..density..", 
            fill = "steelblue",bins = 9, add_density = TRUE)

gghistogram(df_pupae_97, x = "hours", y = "..density..", 
            fill = "steelblue",bins = 9, add_density = TRUE)

#failure of shapiro wilk normality test means we should do wilcoxon test

df_pupae_92 %>%
  group_by(galbut) %>%
  get_summary_stats(hours, type = "median_iqr")

df_pupae_97 %>%
  group_by(galbut) %>%
  get_summary_stats(hours, type = "median_iqr")

stat.test.92 <- df_pupae_92 %>% 
  wilcox_test(hours ~ galbut) %>%
  add_significance()
stat.test.92

stat.test.97 <- df_pupae_97 %>% 
  wilcox_test(hours ~ galbut) %>%
  add_significance()
stat.test.97

#these plots are to check that I ran wilcoxon correctly. ggpubr runs wilcoxon here too
#p values match
p_pupae_92 <- ggplot(df_pupae_92, aes(strain, hours)) +
  geom_violin(aes(fill = galbut))+
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))
p_pupae_92

p_pupae_97 <- ggplot(df_pupae_97, aes(strain, hours)) +
  geom_violin(aes(fill = galbut))+
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))
p_pupae_97


#plot it
#using the violin plot to show distribution of data
#alternative is box plot but because of many pupae eclosing on the same day, data is lost

p_pupae <- ggplot(df_pupae, aes(galbut, hours)) +
  geom_violin(aes(fill = galbut))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(72, 168), 
                     #breaks = seq(96, 154, by = 24),
                     breaks = c(72, 96, 120, 144, 168)) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))

p_pupae

p_pupae2 <- ggplot(df_pupae, aes(galbut, hours, fill = galbut)) +
  geom_boxplot(aes(fill = galbut, alpha = 0.9))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(72, 168), 
                     #breaks = seq(96, 154, by = 24),
                     breaks = c(72, 96, 120, 144, 168)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge()) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))

p_pupae2

p_pupae3 <- ggplot(df_pupae, aes(galbut, hours, fill = galbut)) +
  #geom_boxplot(aes(fill = galbut, alpha = 0.9))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(72, 168), 
                     #breaks = seq(96, 154, by = 24),
                     breaks = c(72, 96, 120, 144, 168)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge(jitter.height = 0.8)) +
  stat_summary(fun = median, 
               geom = "crossbar",
               width = 0.5,
               position = position_dodge(0.8),
               aes(group = interaction(strain, galbut))) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))

p_pupae3


ggsave("developmental_pupae.pdf", width = 5.2, height = 3.5, units = "in")

#get summary stats
df_pupae %>% group_by(strain, galbut) %>% summarise(median = median(hours), avg = mean(hours))


#######ADULT ANALYSIS#######


#read in the data
df_adults <- read_excel("development_data_adults.xlsx", sheet = "plot_data_combined")

#change strain # to DGRP name
df_adults$strain <- str_replace(df_adults$strain, "25192", "DGRP-399")
df_adults$strain <- str_replace(df_adults$strain, "25197", "DGRP-517")

#filtering to only have both for looking at right statistics
#df_adults_both <- df_adults %>% filter(sex == "both")

#do normality testing to know wilcoxon vs t test
#doesn't appear to be normalized, do wilcoxon
# ggplot(df_adults_both, aes(x =hours)) + 
#   geom_histogram(bins = 15) +
#   facet_wrap(~strain)
#filter for downstream stats and for testing normality
df_adults_92 <- df_adults %>% filter(strain == "DGRP-399")
df_adults_97 <- df_adults %>% filter(strain == "DGRP-517")

library(car)
qqp(df_adults_92$hours)
qqp(df_adults_97$hours)

ggdensity(df_adults_92$hours)
ggdensity(df_adults_97$hours)

#25192 shapiro wilk normality test
#both failed (p < 0.001 inferring not normally distributed)
# Shapiro-Wilk normality test for infected development time
with(df_adults_92, shapiro.test(hours[galbut == "TRUE"]))# p = 1.125 e-15
# Shapiro-Wilk normality test for uninfected development time
with(df_adults_92, shapiro.test(hours[galbut == "FALSE"])) # p = 6.164e-15

#25197 shapiro wilk normality test
#both failed (p < 0.001 inferring not normally distributed)
# Shapiro-Wilk normality test for infected development time
with(df_adults_97, shapiro.test(hours[galbut == "TRUE"]))# p = 4.266e-16
# Shapiro-Wilk normality test for uninfected development time
with(df_adults_97, shapiro.test(hours[galbut == "FALSE"])) # p = 1.121e-10

# #remove lines with no fly counts
# df_adults_filt <- df_adults %>% filter(fly_counts > 0)
# 
# #remove both as a sex variable
# df_adults_filt <- df_adults_filt %>% filter(sex != "both")

p_adults <- ggplot(df_adults, aes(sex, hours)) +
  geom_violin(aes(fill = galbut))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(192, 336), 
                     breaks = c(192, 216, 240, 264, 288, 312, 336)) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  stat_compare_means(aes(group = galbut))

p_adults

p_adults2 <- ggplot(df_adults, aes(sex, hours, fill = galbut)) +
  geom_boxplot(aes(fill = galbut, alpha=0.9))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(192, 336), 
                     breaks = c(192, 216, 240, 264, 288, 312, 336)) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.8)) +
  stat_compare_means(aes(group = galbut))
  
p_adults2


p_adults3 <- ggplot(df_adults, aes(sex, hours, fill = galbut)) +
  #geom_boxplot(aes(fill = galbut, alpha=0.9))+
  facet_wrap(~strain) +
  theme_bw() +
  scale_y_continuous(limits = c(192, 336), 
                     breaks = c(192, 216, 240, 264, 288, 312, 336)) +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.8)) +
  stat_summary(fun = median, 
               geom = "crossbar",
               width = 0.5,
               position = position_dodge(0.8),
               aes(group = interaction(sex, galbut))) +
  stat_compare_means(aes(group = galbut))

p_adults3

ggsave("developmental_adults.pdf", width = 5.2, height = 3.5, units = "in")

#get summary stats
df_adults %>% group_by(strain, galbut, sex) %>% summarise(median = median(hours), avg = mean(hours))


#####ANALYSIS BY STRAIN ONLY--NO GALBUT VIRUS#######

#check pupation differences by strain (no galbut virus)
df_pupae_uninfect <- df_pupae %>% filter(galbut == FALSE)

p_pupae_uninfect <- ggplot(df_pupae_uninfect, aes(strain, hours, fill = strain)) +
  #geom_boxplot(aes(fill = strain, alpha = 0.9))+
  facet_wrap(~galbut) +
  theme_bw() +
  scale_y_continuous(limits = c(72, 168), 
                     breaks = c(72, 96, 120, 144, 168)) +
  scale_fill_manual(values = c("#009E73", "#CC79A7")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.8)) +
  stat_summary(fun = median, 
               geom = "crossbar",
               width = 0.5,
               position = position_dodge(0.8),
               aes(group = interaction(strain))) +
  theme(legend.position = "none") +
  stat_compare_means(aes(group = strain))

p_pupae_uninfect

ggsave("developmental_pupae_uninfected_top-bar.pdf", width = 1.8, height = 3.5, units = "in")


#get summary stats
df_pupae_uninfect %>% group_by(strain) %>% summarise(median = median(hours), avg = mean(hours))


#look to see total development time difference between strain development
df_adults_uninfect <- df_adults %>% filter(galbut == FALSE)

p_adults_uninfect <- ggplot(df_adults_uninfect, aes(sex, hours, fill = strain)) +
  #geom_boxplot(aes(fill = strain, alpha = 0.9))+
  facet_wrap(~galbut) +
  theme_bw() +
  scale_y_continuous(limits = c(192, 336), 
                     breaks = c(192, 216, 240, 264, 288, 312, 336)) +
  scale_fill_manual(values = c("#009E73", "#CC79A7")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.8)) +
  stat_summary(fun = median, 
               geom = "crossbar",
               width = 0.5,
               position = position_dodge(0.8),
               aes(group = interaction(strain))) +
  theme(legend.position = 'none') +
  stat_compare_means(aes(group = strain))

p_adults_uninfect

ggsave("developmental_adults_uninfected_top-bar.pdf", width = 1.8, height = 3.5, units = "in")


#get summary stats
df_adults_uninfect %>% group_by(strain, sex) %>% summarise(median = median(hours), avg = mean(hours))

