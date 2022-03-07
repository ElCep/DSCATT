library("ggplot2")

setwd("~/github/DSCATT/toymodel/")

data.df <- read.csv("results_pse_2/population350.csv", header = T)

ggplot(data = data.df, aes(x = objective.deltaPopulation, y = objective.deltaFertilite))+
  geom_point(aes(size = evolution.samples, color = strategiePaturage) )+
  geom_rect(mapping= aes(xmin =0.0, 
                    xmax = 0.2, 
                    ymin = 0.50, 
                    ymax=0.75), color="black", alpha=0.0)+
  geom_rect(mapping= aes(xmin =0.0, 
                         xmax = 0.2, 
                         ymin = 0.0, 
                         ymax=0.25), color="black", alpha=0.0)


sel <- data.df$objective.deltaPopulation > 0.0 & data.df$objective.deltaFertilite > 0.5
subdata <- data.df[sel,]


sel <- data.df$objective.deltaPopulation > 0.0 & data.df$objective.deltaFertilite < 0.25 & data.df$objective.deltaFertilite >= 0.0
subdata <- data.df[sel,]
