#Starvation Assay analysis
#This script plots the starvation assay data

#Date: 02-19-20
#Created by: Shaun Cross

#Import the necessary libs
#Load appropriate packages
library(tidyverse)
library(googlesheets) #this isn't working at the moment...
library(readxl)

#read in the data (From google sheet)
#df <- gs_ls("Starvation_Assay")


#Set wd to the location the script file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in the data (exported excel)
df <- read_excel("starvation_assay.xlsx")

df$Hours <- as.factor(df$Hours)
df$Strain <- as.factor(df$Strain)
df_avg <- df %>% group_by(Strain, Infected, Sex, Replicate, Hours) %>% summarize(avg_alive = mean(Percent_alive), std_alive = sd(Percent_alive))

#Plot it
ggplot(df_avg, aes(x = Hours, y = avg_alive, group = interaction(Strain, Infected, Sex))) +
  geom_point(aes(shape=factor(Sex))) +
  geom_line(aes(color=Infected, linetype = )) +
  geom_errorbar(aes(ymin = (avg_alive - std_alive), ymax = (avg_alive + std_alive)), 
                width = 0.2, 
                color = "grey50",
                size = 0.2) +
  facet_grid(Sex~Strain) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Hours since food") + 
  ylab("Mean percent flies alive per vial")


ggplot(df_avg, aes(x = Hours, y = avg_alive, group = interaction(Strain, Infected, Sex))) +
  geom_jitter(aes(Hours, avg_alive, color = Infected), data = df_avg, 
              position = position_jitter(width = 0.1)) +
  facet_grid(Sex~Strain + Infected)# +
  #geom_crossbar(data=df_avg,aes(x=tt,ymin=val, ymax=val,y=val,group=tt), width = 0.5)
  #scale_x_continuous(breaks=c(1,2,3),labels=c("Group1", "Group2", "Group3"))

#Summary statistics
df_avg <- df %>% group_by(Strain, Infected, Sex, Hours) %>% summarize(avg_alive = mean(Percent_alive), std_alive = sd(Percent_alive))

#box plot with multiple categorical variables
install.packages("ggthemes")
library(ggthemes)
g <- ggplot(df_avg, aes(Hours, avg_alive))
g + geom_boxplot(aes(fill=factor(Infected))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Flies alive over hours of starvation",
       x="Hours",
       y="Average percent flies alive") +
  facet_grid(Sex~Strain) +
  theme_bw() +
  theme(panel.grid = element_blank())

#Try it out with some stats with ggpubr
library(ggpubr)
#Lots of good links for getting these plots made:
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/#stat_compare_means
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/79-plot-meansmedians-and-error-bars/
#https://www.rdocumentation.org/packages/ggpubr/versions/0.2.5/topics/stat_compare_means

#have to do individually
df_25197_M <- df_avg %>% filter(Strain == "25197", Sex == "M")
df_25197_F <- df_avg %>% filter(Strain == "25197", Sex == "F")
df_25192_M <- df_avg %>% filter(Strain == "25192", Sex == "M")
df_25192_F <- df_avg %>% filter(Strain == "25192", Sex == "F")

p_97M <- df_25197_M %>% ggboxplot(x = "Hours", y = "avg_alive",
                               color = "Infected", add = "mean_se",
                               palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.format")

p_97M #some not significant? seems strange...

p_97F <- df_25197_F %>% ggboxplot(x = "Hours", y = "avg_alive",
                                  color = "Infected", add = "mean_se",
                                  palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.format")

p_92M <- df_25192_M %>% ggboxplot(x = "Hours", y = "avg_alive",
                                  color = "Infected", add = "mean_se",
                                  palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.format")

p_92F <- df_25192_F %>% ggboxplot(x = "Hours", y = "avg_alive",
                                  color = "Infected", add = "mean_se",
                                  palette = "jco"
) +
  stat_compare_means(aes(group = Infected), label = "p.format")

#merge together
library(cowplot)

plot_grid(p_97M, p_97F, p_92M, p_92F, labels = c("25197 M", "25197 F", "25192 M", "25192 F"))


# Box plot facetted by "hour". Only for 25197M to verify stats
p <- ggboxplot(df_25197_M, x = "Infected", y = "avg_alive",
               color = "Infected", palette = "jco",
               facet.by = "Hours", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")
