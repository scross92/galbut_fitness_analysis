#This script is to analyze the fitness of flies infected with galbut virus against DCV

#Created by: Shaun Cross
#Date: 03 Feb 2020

#Set wd to the location the file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load appropriate packages
library(tidyverse)
library(readxl)
library(lubridate)

#Load in the excel file
df <- read_excel("DCV_survival_tidy.xlsx")

#average triplicates across technical duplicates
df_avg <- df %>% group_by(Strain, Infected, Sex, Group, Day, technical_rep) %>% summarize(avg_alive = mean(Percent_alive), std_alive = sd(Percent_alive))

#plot it

ggplot(df_avg, aes(x = Day, y = avg_alive, group = interaction(Strain, Infected, Sex, Group))) +
  geom_point(aes(shape=factor(Sex))) +
  geom_line(aes(color=Infected, linetype = Group)) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), 
                width = 0.2, 
                color = "grey50",
                size = 0.2) +
  facet_grid(Sex~Strain + technical_rep) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Date") + 
  ylab("Mean percent flies alive per vial")


#Plotted with tech replicates combined
df_avg2 <- df %>% group_by(Strain, Infected, Sex, Group, Day) %>% summarize(avg_alive = mean(Percent_alive), std_alive = sd(Percent_alive))


ggplot(df_avg2, aes(x = Day, y = avg_alive, group = interaction(Strain, Infected, Sex, Group))) +
  geom_point(aes(shape=factor(Sex))) +
  geom_line(aes(color=Infected, linetype = Group)) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), 
                width = 0.2, 
                color = "grey50",
                size = 0.2) +
  facet_grid(Sex~Strain) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Date") + 
  ylab("Mean percent flies alive per vial")


#DCV 25192 Only

df_92 <- read_excel("DCV_survival_tidy.xlsx", sheet = "25192_combined")
df_92$Day <- as.factor(df_92$Day)
df_92_avg <- df_92 %>% group_by(Infected, Sex, Group, Day, technical_rep) %>% summarize(avg_alive = mean(Percent_alive), std_alive = sd(Percent_alive))

ggplot(df_92_avg, aes(x = Day, y = avg_alive, group = interaction(Infected, Sex, Group))) +
  geom_point(aes(shape=factor(Sex))) +
  geom_line(aes(color=Infected, linetype = Group)) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), 
                width = 0.2, 
                color = "grey50",
                size = 0.2) +
  facet_grid(Sex~technical_rep) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Date") + 
  ylab("Mean percent flies alive per vial")


#do some stats on this 
#Following from: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
#facet wrap from this thread: https://github.com/kassambara/ggpubr/issues/19
install.packages("ggpubr")
library(ggpubr)

#looks like I can't do the fancy plotting in ggpubr like we can with tidyverse...also may just not know how to do it well at the moment
#going to make a group for each sex and strain
#then we will combine all plots together using cowplot

df_25197_M <- df_avg %>% filter(Strain == "25197", Sex == "M")
df_25197_F <- df_avg %>% filter(Strain == "25197", Sex == "F")
df_25192_M <- df_avg %>% filter(Strain == "25192", Sex == "M")
df_25192_F <- df_avg %>% filter(Strain == "25192", Sex == "F")

ggline(df_25197_M, x = "Day", y = "avg_alive", add = "mean_se",
       color = "Infected", palette = "Group") +
  stat_compare_means(aes(group = Group), label = "p.signif")

#Plots DCV and PBS groups side by side with stats
#25197 Males
p_97M <- df_25197_M %>% ggline(x = "Day", y = "avg_alive",
       color = "Infected", add = "mean_se",
       facet.by = "Group",
       palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.signif")

#25197 Females
p_97F <-df_25197_F %>% ggline(x = "Day", y = "avg_alive",
                      color = "Infected", add = "mean_se",
                      facet.by = "Group",
                      palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.signif")

#25192 Males
p_92M <- df_25192_M %>% ggline(x = "Day", y = "avg_alive",
                      color = "Infected", add = "mean_se",
                      facet.by = "Group",
                      palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.signif")

#25192 Females
p_92F <- df_25192_F %>% ggline(x = "Day", y = "avg_alive",
                      color = "Infected", add = "mean_se",
                      facet.by = "Group",
                      palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.signif")


#let's combine them into one master plot just to make it easier to send to everybody
library(cowplot)

plot_grid(p_97M, p_97F, p_92M, p_92F, labels = c("25197 M", "25197 F", "25192 M", "25192 F"))

