library(ggplot2)
library(dplyr)

setwd("~/dev/DSCATT/img_article_JASSS/")

rainfall <- seq(from=0,to=1000)
FYP <- function(rain){
  if (rain < 317) return(0)
  if (rain >= 317 & rain <= 805) return(1860.8*log(rain)-8675.6)
  if(rain > 805) return(3775)
} 
full_yield <- lapply(rainfall, FYP) %>% unlist()
df_fullpotential <- data.frame(rainfall, full_yield)


potential_mil_yieldplot<-ggplot(df_fullpotential, aes(x=rainfall, y=full_yield))+
  geom_line(color="darkcyan")+ 
  theme_light()+
  xlab("rainfall in mm")+
  ylab("full potential \n Millet yield (kg/ha)")
potential_mil_yieldplot

navail <- seq(from=0,to=100)
NRF <- function(navail){
  if (navail < 18) return(0.25)
  if (navail >= 18 & navail < 83) return(0.501)
  if(navail >= 83) return(1)
} 
NRFvalues <- lapply(navail, NRF) %>% unlist()
df_NRF <- data.frame(navail, NRFvalues)


nrfplot <- ggplot(df_NRF, aes(x=navail, y=NRFvalues))+
  geom_line(color="darkcyan")+ 
  theme_light()+
  xlab("Nitrogen available (kg/ha)")+
  ylab("NRF (dimensionless")
nrfplot




