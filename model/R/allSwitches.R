library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(stringr)
library(RColorBrewer)

buildImage = function(dynamicName, dirPath) {
  fifi = paste0(dirPath, dynamicName,".csv")
  df = read.csv(fifi, header = T, sep=",", fileEncoding = "UTF-8", check.names = F)
  #i = 1
  
  traceColumn = function(i){
    dyn = df[,i]
    type = rep(names(df)[i],times = length(dyn))
    trace = t(rbind(type, dyn,1:length(dyn))) %>% data.frame
    return(trace)
  }
  
  traceList = lapply(1:ncol(df), traceColumn)
  all = do.call(rbind, traceList) 
  names(all)=c("variable","value", "years")
  all$value = as.numeric(all$value) 
  all$years = as.numeric(all$years)
  
  # ploplot = ggplot(all, aes(x=years))+
  #   geom_line( aes(y=value, color=variable), linewidth = 0.6)+
  #   theme_light()+
  #   xlab("Years")+
  #   ylab(dynamicName)+
  #   geom_vline(xintercept=25, linetype='dotted', col = 'black')+
  #   labs(color="Scenario")
  custom_pal<-  c("black", #base
                  "saddlebrown", #mulching
                  "springgreen4", #tress
                  "green1", #grazing
                  "hotpink", #LSU1
                  "darkmagenta", #LSU full
                  "gold1", #No solidarity     
                  "turquoise2", #Rainfall 
                  "red2",# rotation
                  "navy" #own fallow
                  )
  #custom_pal <-  c("black", brewer.pal(9, "Set1"))
 
   # base onmly dataframe
  baseline <- all %>%  filter(variable=="Base")
  all<- filter(all,years>23)  
  ploplot = ggplot(all, aes(x=years))+
    geom_line( aes(y=value, color=variable,), linewidth = 0.6)+
    geom_line(data = baseline, aes(y=value, color=variable,), linewidth = 0.4)+
    theme_light()+
    xlab("Years")+
    ylab(dynamicName)+
    geom_vline(xintercept=25, linetype='dotted', col = 'grey15')+
    scale_colour_manual(values=custom_pal)+
    labs(color="Scenario")
  
  pngFileName = str_replace(fifi,".csv","BIF_new.png")
  
  ggsave(pngFileName, ploplot, width=1500, height = 891, units = "px", dpi=150)
}


dynamicNames = c("yqs","rqs","nitrogen", "effective fallow", "SQ x Nitrogen", "population",
                 "herd size", "millet yield", "loan", "foodStress" )
lapply(dynamicNames, buildImage, "~/tmp/data_courbes_bifurcations/")



fifi = "~/DSCATT/img_article_JASSS/data_courbes_bifurcations/population.csv"
df = read.csv(fifi, header = T, sep=",", fileEncoding = "UTF-8", check.names = F)

traceColumn = function(i){
  dyn = df[,i]
  type = rep(names(df)[i],times = length(dyn))
  trace = t(rbind(type, dyn,1:length(dyn))) %>% data.frame
  return(trace)
}

traceList = lapply(1:ncol(df), traceColumn)
all = do.call(rbind, traceList) 
names(all)=c("variable","value", "years")
all$value = as.numeric(all$value) 
all$years = as.numeric(all$years)
all$variable = as.factor(all$variable)

all$variable %>% as.factor() %>% levels


all %>%  summary


#custom color palette : grey then standard discret ggplot2 palette
# to show : scales::show_col(c("#7f7f7f",hue_pal()(9)))
ppp = ggplot(all, aes(x=years))+
  geom_line( aes(y=value, color=variable,), linewidth = 0.6)+
  geom_line(data = baseline, aes(y=value, color=variable,), linewidth = 0.6)+
  theme_light()+
  xlab("Years")+
  ylab("pop")+
  geom_vline(xintercept=25, linetype='dotted', col = 'black')+
  scale_colour_manual(values=custom_pal)+
  labs(color="Scenario")
ppp






