library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(stringr)

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
  custom_pal<-  c("#7a7a7a",hue_pal()(9))
  # base onmly dataframe
  baseline <- all %>%  filter(variable=="Base")
  
  ploplot = ggplot(all, aes(x=years))+
    geom_line( aes(y=value, color=variable,), linewidth = 0.6)+
    geom_line(data = baseline, aes(y=value, color=variable,), linewidth = 0.6)+
    theme_light()+
    xlab("Years")+
    ylab("pop")+
    geom_vline(xintercept=25, linetype='dotted', col = 'black')+
    scale_colour_manual(values=custom_pal)+
    labs(color="Scenario")
  
  pngFileName = str_replace(fifi,".csv",".png")
  ggsave(pngFileName, ploplot, width=1500, height = 891, units = "px", dpi=150)
}

dynamicNames = c("yqs","rqs","nitrogen", "effective fallow", "SQ x Nitrogen", "population",
                 "herd size", "millet yield", "loan", "foodStress" )
lapply(dynamicNames, buildImage, "~/dev/DSCATT/img_article_JASSS/data_courbes_bifurcations/")



fifi = "~/dev/DSCATT/img_article_JASSS/data_courbes_bifurcations/population.csv"
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
all$variable = as.factor()

all$variable %>% as.factor() %>% levels



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






