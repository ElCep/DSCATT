library("dplyr")
library("ggplot2")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.csv("../data_simu/replication.csv", header = T, sep = ",")


# on va tirer 10 fois le même nombre de sample 
# créer des groupes dans un data frame
# et on plotera les boxplot correspondant par groupe

sample.v <- seq(from = 2, to = 10, by = 2) # vecteur du nombre de tirage

df.gp <- data.frame()
for(i in sample.v){
  for(j in 1:5){
    a <- df %>% slice_sample(n = i, replace = T)
    a$gp <- j
    df.gp <- rbind(df.gp, a)
  }
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = ef))+
    labs(x  = "sample", title = paste("nombre de réplication:", i))+
    ylim(c(0.30,0.50))+
    theme_bw()
  ggsave(paste0("../img/sample/ef_sample_",i,".png"))
  
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = pop))+
    labs(x  = "sample", title = paste("nombre de réplication:", i))+
    # ylim(c(0.30,0.50))+
    theme_bw()
  ggsave(paste0("../img/sample/pop_sample_",i,".png"))
  
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = yield))+
    labs(x  = "sample", title = paste("nombre de réplication:", i))+
    ylim(c(600,750))+
    theme_bw()
  ggsave(paste0("../img/sample/yield_sample_",i,".png"))
  
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = herd))+
    labs(x  = "sample", title = paste("nombre de réplication:", i))+
    ylim(c(50,100))+
    theme_bw()
  ggsave(paste0("../img/sample/herd_sample_",i,".png"))
}
