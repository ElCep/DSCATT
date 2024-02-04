setwd("~/Documents/CIRAD/2021/DSCATT/data/obs/")
library('haven')
library('labelled')
library('ggplot2')
library('date')
library("dplyr")

data.df <- unlabelled(read_dta("~/dev/DSCATT/model/R/data/Residents_ES_Etienne Delay_Diohine.dta"))

sel1 <- data.df$EventCode == "BTH" 
sel2 <- data.df$EventCode == "DTH" | data.df$EventCode == "OMG"

#naissance
bth <- data.df[sel1,]
bth$year <- format(bth$EventDate, format = "%Y")
s <- bth %>%
      count(year)

naissance_by_year <- bth %>%
  count(year)

data.df$EventCode %>%  levels



#deces sortie du system
dth <- data.df[sel2,]
dth$year <- format(dth$EventDate, format = "%Y")

sorties_by_year <-  dth %>%  count(year)
sorties_by_year


names(sorties_by_year ) <-  c("year", "sorties")
names(naissance_by_year) <-  c("year", "naissances")


movement_by_year <-  inner_join(sorties_by_year, naissance_by_year)
movement_by_year

movement_by_year$solde <-  movement_by_year$naissances - sorties_by_year$sorties

cumsum(movement_by_year$solde)


mean(movement_by_year$solde)


ggplot(data=movement_by_year,aes( x =  as.numeric(as.character(year)), y = naissances))+
  geom_path()


ggplot(data=movement_by_year,aes( x =  as.numeric(as.character(year)), y = cumsum(solde)))+
  geom_path()


ggplot(data = s, aes(x =  as.numeric(as.character(year)), y = n))+
  geom_point()+
  geom_path()+
  theme_bw()+
  labs(x = "année", y = "nombre de naissances")
