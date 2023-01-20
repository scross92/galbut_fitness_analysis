library(tidyverse)
library(readxl)
library(lubridate)
library(survival)

# Analyze Fitness Experiment #1 Lifespan data

#Created by Mark Stenglein and Shaun Cross
#Date: 5-6-20

#Set wd to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Questions:
# (1) Do flies die faster or slower when infected with galbut virus?

#
# Expterimental setup and data processing notes: 
#
#
# Expt'al setup:
# For strains 25192 (DGRP-399) and 25197 (DGRP-517): 
# For each of these strains, there was a galbut-infected and a galbut-uninfected sub-strain
# For each sub-strain: 5 replicate vials were setup with 5 M and 5 F adults that were newly eclosed and virgins
# Every 3 days these original flies were transferred to new vials.
# Every time a fly was noted to be dead, the death was recorded 

# The data is found with the following txt file: "Lifespan_complete_expt_both_tidy_all_cleaned.txt"
#
# The number_valid_flies column is 10 - the number of flies flagged for removal from analysis because they died of 'non-natural' causes
# (i.e. they died during vial transfer)
#
# read file:
df <- read.delim("Lifespan_complete_expt_both_tidy_all_cleaned.txt", sep="\t", header=TRUE)

# rename BDSC strains IDs with DGRP names
df$strain <- str_replace(df$strain, "25192", "DGRP-399")
df$strain <- str_replace(df$strain, "25197", "DGRP-517")

# calculate fractional value of flies remaining
# since not all vials had the same # of 'valid' flies
df <- df %>% mutate (fraction_alive = number_alive / number_valid_flies)

# average replicates
df_avg <- df %>% group_by(strain, galbut, day) %>% summarize(avg_alive = mean(fraction_alive), std_alive = sd(fraction_alive)) 

# plot individual replicates
p_individual <- ggplot(df, aes(x=day,
               y=fraction_alive, 
               group=interaction(strain, galbut, replicate)))  + 
  # geom_point() +
  geom_line(aes(color=galbut)) +
  # geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), width = 0.2, color="grey50", size=0.2) +
  facet_wrap(~strain) +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  xlab("Days post eclosion") + 
  ylab("Fraction flies alive per vial")

p_individual
 
 
# plot average of replicates
p_avg <- ggplot(df_avg, aes(x=day,
               y=avg_alive, 
               group=interaction(strain, galbut)))  + 
  # geom_point() +
  geom_line(aes(color=galbut)) +
  # geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), width = 0.5, color="grey50", size=0.2) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive), color=galbut), width = 0.5, size=0.2) +
  facet_wrap(~strain) +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  scale_color_manual(values = c("#56B4E9", "#D55E00")) +
  xlab("Days post eclosion") + 
  ylab("Fraction flies alive per vial")
 
p_avg

ggsave("lifespan_avg.pdf", height=4, width=6, units="in")


#plot all, but only galbut virus uninfected flies. This is to compare lifespan by strain alone.

#first filter out galbut virus infected flies

df_filt <- df %>% filter(galbut == FALSE)

df_filt_avg <- df_filt %>% group_by(strain, day) %>% summarize(avg_alive = mean(fraction_alive), std_alive = sd(fraction_alive)) 


p_uninfect <- ggplot(df_filt_avg, aes(x=day,
                            y=avg_alive))  + 
  geom_line(aes(color=strain)) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive), color=strain), width = 0.5, size=0.2) +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("Days post eclosion") + 
  ylab("Fraction flies alive per vial")

p_uninfect

ggsave("lifespan_uninfected_avg.pdf", height=4, width=6, units="in")

# is survival statistically different between galbut +/- groups
# see: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html 
# for an excellent introduction to survival analysis in R
# see also: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
df_surv <- read_excel("Lifespan_complete_expt_tidy_data-5-6-20.xlsx", sheet = "both")

df_surv$Strain <- str_replace(df_surv$Strain, "25192", "DGRP-399")
df_surv$Strain <- str_replace(df_surv$Strain, "25197", "DGRP-517")

df_surv_399 <- filter(df_surv, Strain == "DGRP-399")
df_surv_517 <- filter(df_surv, Strain == "DGRP-517")

sd_399 <- survdiff(Surv(Lifespan, !Remove) ~ Galbut, data = df_surv_399)
sd_517 <- survdiff(Surv(Lifespan, !Remove) ~ Galbut, data = df_surv_517)

# this implements a log-rank test or Mantel-Haenszel test in the survival package in R
# citation: Harrington, D. P. and Fleming, T. R. (1982). A class of rank test procedures for censored survival data. Biometrika 69, 553-566.
# survival package citation: https://cran.r-project.org/web/packages/survival/citation.html

#survival based on galbut virus
#no/minimal effect (399 p-val = 0.0803, 517 p-val = 0.588)

1 - pchisq(sd_399$chisq, length(sd_399$n) - 1)
1 - pchisq(sd_517$chisq, length(sd_517$n) - 1)

# Surival based on strain...
# yes!  huge effect (p=4.8e-4)
sd_strain <- survdiff(Surv(Lifespan, !Remove) ~ Strain, data = df_surv)
1 - pchisq(sd_strain$chisq, length(sd_strain$n) - 1)

# quick plot of survival based on strain
ggplot() + geom_jitter(data=filter(df_surv, Remove==FALSE), aes(x=as.factor(Strain), y=Lifespan, fill=Galbut), shape=21, width=0.1)

# what are average lifespans?
# "censor" (remove) flies that died during transfer
df_surv <- filter(df_surv, Remove != TRUE)
df_surv %>% group_by(Strain, Galbut) %>% summarize(mean_lifespan = mean(Lifespan))
df_surv %>% group_by(Strain, Galbut) %>% summarize(median = median(Lifespan))

#by strain only
df_surv %>% filter(Galbut == FALSE) %>% group_by(Strain) %>% summarize(median = mean(Lifespan))

# Surival based on multiple co-variates:
# use Cox regression model
# strain, sex, galbut infection status
# see: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#the_cox_regression_model

sd_all <- coxph(Surv(Lifespan, !Remove) ~ Strain + Galbut, data = df_surv)
# all have significant effects...
sd_all
