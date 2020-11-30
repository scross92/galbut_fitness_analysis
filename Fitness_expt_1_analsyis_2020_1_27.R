library(tidyverse)
library(readxl)
library(lubridate)
library(survival)

# Analyze Fitness Experiment #1 Lifespan data

# Questions:
# (1) Do flies die faster or slower when infected with galbut virus?

#
# Expterimental setup and data processing notes: 
#
#
# Expt'al setup:
# For strains 25192 (DGRP-399) and 25197 (DGRP-517): 
# For each of these strains, there was a galbut-infected and a galbut-uninfected sub-strain
# For each sub-strain: 5 replicate vials were setup with 5 M and 5 F adults (a few days post eclosion?)
# Every 3 days these original flies were transferred to new vials.
# Every time a fly was noted to be dead, the death was recorded 
#
# Shaun, Ali, and Tillie are recording when individual flies die in a google spreadsheet, here:
# https://docs.google.com/spreadsheets/d/1GRPyScyD4YK8LGHbXkpnjkVacb3gZCfW0kjQn3PwqsU/edit#gid=329500492
#
# I downloaded this data and put it into an Excel spreadsheet: Lifespan_expt_1_tidy_data.xlsx
# in a slightly cleaned up format.  Exported this as Lifespan_expt_1_tidy_data.txt  
#
# Note Excel exports as an annoying \r\n carriage-return newline format.  Need to convert this to normal newlines:
# This perl one-liner does this in place:
# perl -pi -e 's/\r\n/\n/g' Lifespan_expt_1_tidy_data.txt

# This flies dying data is a sparse version of what I'd like to plot, which is the % of flies alive in each vial at each day.
# To convert this data into a format that shows %flies alive, I wrote a perl script: fill_out_lifespan_data, and ran it as follows:
# ./fill_out_lifespan_data Lifespan_expt_1_tidy_data.txt > Lifespan_expt_1_tidy_data_all.txt
#
# This outputs a table with these columns: 
# strain	galbut	day	replicate	number_alive number_valid_flies
#
# The number_valid_flies column is 10 - the number of flies flagged for removal from analysis because they died of 'non-natural' causes
# (i.e. they died during vial transfer)
#
# read file:
df <- read.delim("Lifespan_expt_1_tidy_data_all.txt", sep="\t", header=TRUE)

# rename BDSC strains IDs with DGRP names
df$strain <- str_replace(df$strain, "25192", "DGRP-399")
df$strain <- str_replace(df$strain, "25197", "DGRP-517")

# calculate fractional value of flies remaining
# since not all vials had the same # of 'valid' flies
df <- df %>% mutate (fraction_alive = number_alive / number_valid_flies)

# average replicates
# df_avg <- df %>% group_by(Strain, Galbut, Sex, Date) %>% summarize(avg_alive = mean(number_alive), std_alive = sd(number_alive)) 
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
  xlab("Days post eclosion") + 
  ylab("Fraction flies alive per vial")
 
p_avg

ggsave("lifespan_avg.pdf", height=4, width=6, units="in")

# plot average plus individuals
p_all <- ggplot(data=df_avg) +
  geom_line(aes(x=day,
                y=avg_alive, 
                group=interaction(strain, galbut),
                color=galbut)) +
  geom_errorbar(aes(x=day, ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), width = 0.2, color="grey50", size=0.2) +
  geom_line(data = df, aes(x=day,
                    y=fraction_alive, 
                    group=interaction(strain, galbut, replicate),
                    color=galbut), size=0.25, linetype=2) +
  facet_wrap(~strain) +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  xlab("Days post eclosion") + 
  ylab("Fraction flies alive per vial")

p_all


p_individual
p_avg

# is survival statistically different between galbut +/- groups
# see: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html 
# for an excellent introduction to survival analysis in R
# see also: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

# this is based on that
# TODO: update with updated sex information
df_surv <- read_excel("Lifespan_expt_1_tidy_data.xlsx")

df_surv$Strain <- str_replace(df_surv$Strain, "25192", "DGRP-399")
df_surv$Strain <- str_replace(df_surv$Strain, "25197", "DGRP-517")

# only keep observations with a defined sex.  
# TODO: "censor" (remove) flies with uncertain sex
df_surv <- filter(df_surv, Sex == "M" | Sex == "F")
df_surv_399 <- filter(df_surv, Strain == 399)
df_surv_517 <- filter(df_surv, Strain == 517)

# df_survival <- Surv(df_surv$Lifespan, !df_surv$Remove)

sd_399 <- survdiff(Surv(Lifespan, !Remove) ~ Galbut, data = df_surv_399)
sd_517 <- survdiff(Surv(Lifespan, !Remove) ~ Galbut, data = df_surv_517)

# this implements a log-rank test or Mantel-Haenszel test in the survival package in R
# citation: Harrington, D. P. and Fleming, T. R. (1982). A class of rank test procedures for censored survival data. Biometrika 69, 553-566.
# survival package citation: https://cran.r-project.org/web/packages/survival/citation.html



1 - pchisq(sd_399$chisq, length(sd_399$n) - 1)
1 - pchisq(sd_517$chisq, length(sd_517$n) - 1)

# Surival based on strain...
# yes!  huge effect
sd_strain <- survdiff(Surv(Lifespan, !Remove) ~ Strain, data = df_surv)
1 - pchisq(sd_strain$chisq, length(sd_strain$n) - 1)

# quick plot of survival based on strain
ggplot() + geom_jitter(data=filter(df_surv, Remove==FALSE), aes(x=as.factor(Strain), y=Lifespan, fill=Galbut), shape=21, width=0.1)

# Surival based on sex...
# yes! big effect
sd_sex <- survdiff(Surv(Lifespan, !Remove) ~ Sex, data = df_surv)
1 - pchisq(sd_sex$chisq, length(sd_sex$n) - 1)

# quick plot of survival based on sex
ggplot() + geom_jitter(data=filter(df_surv, Remove==FALSE), aes(x=as.factor(Sex), y=Lifespan, fill=Galbut), shape=21, width=0.1) + 
  facet_wrap(~Strain)

# plot of survival based on sex and galbut infection status
ggplot() + geom_jitter(data=filter(df_surv, Remove==FALSE), aes(x=interaction(as.factor(Sex), Galbut), y=Lifespan, fill=Galbut), shape=21, width=0.1) + 
  facet_wrap(~Strain)


ggplot() + geom_jitter(data=filter(df_surv, Remove==FALSE), aes(x=Galbut, y=Lifespan, fill=Galbut), shape=21, width=0.1) + 
  facet_grid(Sex~Strain)

# what are average lifespans?
df_surv %>% group_by(Strain, Sex, Galbut) %>% summarize(mean_lifespan = mean(Lifespan))
df_surv %>% group_by(Strain, Sex, Galbut) %>% summarize(median = median(Lifespan))

# Surival based on multiple co-variates:
# use Cox regression model
# strain, sex, galbut infection status
# see: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#the_cox_regression_model

sd_all <- coxph(Surv(Lifespan, !Remove) ~ Strain + Galbut + Sex, data = df_surv)
# all have significant effects...
sd_all
