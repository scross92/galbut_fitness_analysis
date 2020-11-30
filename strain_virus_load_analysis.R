#Script for analysis for Figure 1
# Virus load analysis of testes and ovaries compared to bodies

#Aims of this script:

# 1) Plot the virus loads (as determined by virus RNA relative to RpL-32)
# 2) perform a statistical analysis to see if certain sexes have higher virus load

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


#Plot each type by the respective sex
#do wilcoxon test
#showing it on a plot to be able to use tidyverse (ggplot2 with ggpubr)

#separate each plot by sex
#not comparing across sexes, only within sexes

#plot the data altogether (male and female together)

theme_set(theme_classic())
p_load <- ggplot(df_load, aes(as.factor(strain), normalized)) +
  geom_boxplot(aes(fill = factor(strain))) +
  labs(title = "Galbut virus RNA Levels in Adult Flies",
       y = "Galbut virus RNA levels relative to RpL32",
       x = "Strain") +
  scale_fill_manual(values=c("#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous() +
  stat_compare_means() #this naturally chose to do Wilcoxon
p_load #if you want to view the plot

ggsave("strain_virus_load.pdf", width = 4.5, height = 5, units = "in")

df_load %>% group_by(strain) %>% summarise(mean = mean(normalized), median = median(normalized))

####Split the data by sex######

#pull out male data
df_load_m <- df_load %>% 
                filter(sex == 'male')
#calculate mean and median of tissues for finding fold change difference
df_load_m %>% group_by(strain) %>% summarise(mean = mean(normalized), median = median(normalized))

theme_set(theme_classic())
p_load_m <- ggplot(df_load_m, aes(as.factor(strain), normalized)) +
    geom_boxplot(aes(fill = factor(strain))) +
    labs(title = "Galbut Virus RNA Levels in Male Flies",
         y = "Galbut virus RNA levels relative to RpL-32 (2^-deltaCt)",
         x = "Strain") +
    scale_fill_manual(values=c("#009E73", "#CC79A7")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous() +
    stat_compare_means() #this naturally chose to do Wilcoxon
p_load_m #if you want to view the plot

#pull out the female data
df_load_f <- df_load %>% 
    filter(sex == 'female')  

#calculate mean and median of tissues for finding fold change difference
df_load_f %>% group_by(strain) %>% summarise(mean = mean(normalized), median = median(normalized))

p_load_f <- ggplot(df_load_f, aes(as.factor(strain), normalized)) +
  geom_boxplot(aes(fill = factor(strain))) +
  labs(title = "Galbut Virus RNA Levels in Female Flies",
       y = "Galbut virus RNA levels relative to RpL-32 (2^-deltaCt)",
       x = "Strain") +
  scale_fill_manual(values=c("#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous() +
  stat_compare_means()

p_load_f #if you want to visualize the plot here    

#combine the plots
library(patchwork)
p_load_f + p_load_m +plot_layout(ncol=1)

#save it as a PDF
ggsave("sex_strain_virus_load.pdf", width = 5.2, height = 7, units = "in")

