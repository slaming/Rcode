library(openair)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmap)
library(readxl)
library(waclr)
library(ggrepel)
library(ggpubr)

#set working directory 
setwd("~/Desktop/van data/D0014-post bonfire night+leeds/")
#read in file
firework_gc<-read_excel("Firework data.xlsx")%>%
  #select columns to read in
  select(-c(1:18, 20, 22:23, 25:26, 28:29, 31:32, 34:35, 37:38, 40:41, 43:44, 46:47,
            49:50, 52:53, 55:56, 58:59, 61:62, 64:65, 67:68, 70:71, 73:74, 76:77,
            79:80, 82:83, 85:86, 88:89, 91:92, 94:95, 97:98, 100:208 ))%>%
  rename(date = "DateTime")%>%
  rename_all(. %>% tolower())%>%
  mutate(date = ymd_hms(date))


#plotting boxplot as timeseries
#adding labels 
boxplotlabels<-c("23/10/19", "24/10/19", "25/10/19", "26/10/19", "27/10/19", "28/10/19",
                 "29/10/19", "30/10/19", "01/11/19", "02/11/19", "03/11/19",
                 "04/11/19", "05/11/19", "06/11/19", "07/11/19", "08/11/19", "09/11/19",
                 "10/11/19", "11/11/19", "12/11/19", "13/11/19", "14/11/19", "15/11/19", "16/11/19", "17/11/19", "18/11/19", "19/11/19")

#plotting boxplot and assigning to variable 'a'
a<-firework_gc %>% 
  mutate(yday = yday(date),
         yday = as.factor(yday)) %>% 
  filter(!is.na(date)) %>% 
  #replace space in name with "_"
  rename_all(. %>% str_replace(" ", "_"))%>% 
  select(date, yday, `mpxylene_area`) %>% 
  #group_by(yday) %>% 
  ggplot() +
  #decide what to plot(aes(x,y))
  geom_boxplot(aes(yday,mpxylene_area))+
  #add labels to axes
  ylab("[m/p-xylene] / ppbv")+
  xlab("Date")+
  #scale axes
  #ylim(0, 0.75)+
  scale_x_discrete(labels= boxplotlabels)+
  theme_bw()+ 
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 90))

#add line of best fit to box plot
a+geom_smooth(aes(yday,mpxylene_area, group=1), method= loess)