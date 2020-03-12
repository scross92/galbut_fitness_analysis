#DCV Virus Load Assay
#This is to compare to see if there is a difference in virus load (delta Ct) between sex and infection status (gal +/-)

#Created by: Shaun Cross
#2020-03-09

#set wd to saved location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import packages
library(tidyverse)
library(readxl)

#import the data
df <- read_excel("dcv_virus_load.xlsx")

#remove lines that are blank
df_filt <- df %>% na.omit

#group by day

df_avg <- df_filt %>% group_by(infected, day) %>% summarize(avg_ct = mean(delta_ct), std_alive = sd(delta_ct))

res.aov <- aov(delta_ct ~ group, data = df_filt) #calculate residual anova
res.aov

TukeyHSD(res.aov) #Tukey HSD multiple pairwise-comparisons

#pairwise.t.test(df_avg$avg_ct, df_avg$day, p.adjust.method = "BH") #pairwise t-test...but this is just by days not by infection status

  
