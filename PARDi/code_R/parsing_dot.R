library(igraph)
library(stringr)
library(dplyr)



generate_dataframe_and_graph <- function(lignes_du_dot){

acteurs <-  c()
ressources <-  c()
acteurs_colors <-  c()
ressources_colors <-c()
edges <-  c()

for(lili in (lignes_du_dot)){
  if(str_detect(string = lili,pattern = "acteur"))
  {
    
    acteur_name <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[1]
    acteur_name <- acteur_name %>%  str_extract_all(pattern = "\\w+", simplify = T) %>%  str_c( collapse=" ") 
    acteurs <-  c(acteurs,acteur_name)
    
    color <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[2]
    color <- str_split(color, pattern = "pardi=", simplify = T)[1] %>%  as.character %>% str_trim()
    acteurs_colors <-  c(acteurs_colors,color)
  }
  
  if(str_detect(string = lili,pattern = "ressource")){
    
    ressource_name <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[1]
    ressource_name <- ressource_name %>%  str_extract_all(pattern = "\\w+", simplify = T) %>%  str_c( collapse=" ")     
    ressources <-  c(ressources,ressource_name)
    
    color <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[2]
    color <- str_split(color, pattern = "pardi=", simplify = T)[1] %>%  as.character %>% str_trim()
    ressources_colors <-  c(ressources_colors,color)
  }
  
  if(str_detect(string = lili,pattern = "->")){
    
    
    ego <- str_split(lili, "->", simplify = T)[1] %>%  str_extract_all(pattern = "\\w+", simplify = T)%>%  str_c( collapse=" ") 
    
    alter <- str_split(lili, "->", simplify = T)[2]
    alter <-   str_split(alter,pattern = "[:punct:]label=", simplify = T)[1]
    alter <-  alter %>%   str_extract_all(pattern = "\\w+", simplify = T)%>%  str_c( collapse=" ") 
    
    
    color_edge <-  str_split(lili, "->", simplify = T)[2]
    color_edge <-  str_split(color_edge, pattern = "color =", simplify = T)[2]  
    color_edge <- str_split(color_edge, pattern = "pardi=", simplify = T)[1] %>% str_trim()
    color_edge
    
    label_edge <-  str_split(lili, "->", simplify = T)[2]
    label_edge <-  str_split(label_edge, "label=", simplify = T)[2]
    label_edge<-  str_split(label_edge, "color", simplify = T)[1]
    label_edge <-  label_edge %>%   str_extract_all(pattern = "\\w+", simplify = T)%>%  str_c( collapse=" ") 
    label_edge
    
    
    type_et_level <-  str_split(lili, "->", simplify = T)[2] 
    type_et_level <- str_split(type_et_level , pattern = "pardi=",simplify = T)[2]
    pardi_type <- str_split(type_et_level , pattern = "lvl_conflict=",simplify = T)[1]
    pardi_type <- str_extract_all(pardi_type,pattern = "\\w+", simplify = T)%>%  str_c( collapse=" ")
    
    
    conflict_lvl <-  NA
    if(str_detect(string = type_et_level,pattern = "lvl_conflict")){
      conflict_lvl <- str_split(type_et_level , pattern = "lvl_conflict=",simplify = T)[2]
      conflict_lvl <-  str_extract_all(conflict_lvl,pattern = "\\w+", simplify = T)%>%  str_c( collapse=" ")
    }
    
    
    
    
    the_edge <-  c(ego, alter, label_edge, color_edge, pardi_type, conflict_lvl)        
    edges <- c(edges, list(the_edge))
    
  }
  
  
}


edgesdf <- do.call(rbind,edges) %>%  as.data.frame()
names(edgesdf) <-  c("ego", "alter", "label", "color", "pardi_type", "conflict_lvl")
rm(edges)

acteurs_df <- data.frame(acteurs)
names(acteurs_df) <-  "name"
acteurs_df$color <-  acteurs_colors
acteurs_df$type <-  "acteur"
rm(acteurs_colors)

ressources_df <- data.frame(ressources)
names(ressources_df) <-  "name"
ressources_df$color<-  ressources_colors
ressources_df$type <-  "ressource"
rm(ressources_colors)

nodes_df <-  rbind(acteurs_df, ressources_df)
rm(acteurs_df)
rm(ressources_df)

nodes_df$color <-  as.factor(nodes_df$color)

nodes_df$color <-  "white"
edgesdf$color <-  "grey"

nodes_df$ID <-  1:nrow(nodes_df)
gg <- graph_from_data_frame(edgesdf, directed = TRUE, vertices = nodes_df)


return(list(nodes=nodes_df, edges=edgesdf, graph=gg))

}
