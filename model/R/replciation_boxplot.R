library("dplyr")
library("ggplot2")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
df <- read.csv("../data_simu/replication.csv", header = T, sep = ",")


sampleSizes <- seq(1,50, by=5)



df_group <- data.frame()
for(i in sampleSizes){
  for(group in 1:3){
    a <- df %>% slice_sample(n = i, replace = T)
    a$group <- group
    a$size <- i
    df_group <- rbind(df_group, a)
  }
}


ggplot(df_group, aes(group=size, x=size))+
  geom_boxplot(aes(y=ef),orientation = "x")+
  theme_light()

ggplot(df_group, aes(group=size, x=size))+
  geom_boxplot(aes(y=pop),orientation = "x")+
  theme_light()

ggplot(df_group, aes(group=size, x=size))+
  geom_boxplot(aes(y=herd, color= group),orientation = "x")+
  theme_light()+
  facet_grid(rows = vars(group))



ggplot(df_group, aes( x=group))+
  geom_boxplot(aes(y=yield ,  color= group, group=group),orientation = "x")+
  theme_light() +
  facet_grid(rows = vars(size))


df_group$pop %>% unique()
df_group$herd %>% unique()

df_group$size %>% unique()
df_group$group %>% unique()


# on va tirer 10 fois le même nombre de sample 
# créer des groupes dans un data frame
# et on plotera les boxplot correspondant par groupe

sample.v <- seq(from = 2, to = 40, by = 2) # vecteur du nombre de tirage

df.gp <- data.frame()
for(i in sample.v){
  for(j in 1:3){
    a <- df %>% slice_sample(n = i, replace = T)
    a$gp <- j
    df.gp <- rbind(df.gp, a)
  }
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = ef))+
    labs(x  = "group", title = paste("number of simulations sampled:", i))+
    geom_hline(yintercept = 0.3925234, linetype='dotted', col = 'grey')+
    ylim(c(0.30,0.50))+
    theme_bw()
  ggsave(paste0("../img/sample/ef_sample_",i,".png"))
  
  
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = yield))+
    labs(x  = "group", title = paste("number of simulations sampled:", i))+
    geom_hline(yintercept = 689.3767, linetype='dotted', col = 'grey')+
    #ylim(c(689.3767))+
    theme_bw()
  ggsave(paste0("../img/sample/yield_sample_",i,".png"))
  
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = herd))+
    labs(x  = "group", title = paste("number of simulations sampled:", i))+
    geom_hline(yintercept = 89, linetype='dotted', col = 'grey')+
    ylim(c(50,100))+
    theme_bw()
  ggsave(paste0("../img/sample/herd_sample_",i,".png"))
}
