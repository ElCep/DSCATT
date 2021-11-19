library(DiagrammeR)
library(readr)
library(igraph)
library(stringr)


#library(sna)
#AM <- read.dot("~/DSCATT/PARDi/diagram_pardi_simple_edges.dot")
#gg <-  graph_from_adjacency_matrix(AM)
#igraph::plot.igraph(gg,layout=layout_with_kk)
#grViz("~/DSCATT/PARDi/diagram_pardi_simple_edges.dot")

lignes_du_dot <-  read_lines("~/DSCATT/PARDi/diagram_pardi_simple_edges.dot")

acteurs <-  c()
ressources <-  c()
acteurs_colors <-  c()
ressources_colors <-c()
edges <-  c()

for(lili in (lignes_du_dot)){
  if(str_detect(string = lili,pattern = "acteur"))
  {
    cat("ACTEUR\n")
    acteur_name <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[1]
    acteur_name <- acteur_name %>%  str_extract_all(pattern = "\\w+", simplify = T) %>%  str_c( collapse=" ") 
    acteurs <-  c(acteurs,acteur_name)
    
    color <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[2]
    color <- str_split(color, pattern = "pardi=", simplify = T)[1] %>%  as.character %>% str_trim()
    acteurs_colors <-  c(acteurs_colors,color)
  }
  
  if(str_detect(string = lili,pattern = "ressource")){
    cat("RESSOURCE\n")
    ressource_name <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[1]
    ressource_name <- ressource_name %>%  str_extract_all(pattern = "\\w+", simplify = T) %>%  str_c( collapse=" ")     
    ressources <-  c(ressources,ressource_name)

    color <-  str_split(string = lili,pattern = "[:punct:]color =", simplify = T)[2]
    color <- str_split(color, pattern = "pardi=", simplify = T)[1] %>%  as.character %>% str_trim()
    ressources_colors <-  c(ressources_colors,color)
  }
  
  if(str_detect(string = lili,pattern = "->")){
    
    
    cat("EDGE\n")
        
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
    
    
    the_edge <-  c(ego, alter, label_edge, color_edge)        
    edges <- c(edges, list(the_edge))
    
  }
  
  
}

edgesdf <- do.call(rbind,edges) %>%  as.data.frame()
names(edgesdf) <-  c("ego", "alter", "label", "color")
rm(edges)

acteurs_df <- data.frame(acteurs)
names(acteurs_df) <-  "ID"
acteurs_df$color <-  acteurs_colors
acteurs_df$type <-  "acteur"
rm(acteurs_colors)

ressources_df <- data.frame(ressources)
names(ressources_df) <-  "ID"
ressources_df$color<-  ressources_colors
ressources_df$type <-  "ressource"
rm(ressources_colors)


nodes_df <-  rbind(acteurs_df, ressources_df)
rm(acteurs_df)
rm(ressources_df)

nodes_df$color <-  as.factor(nodes_df$color)

library(dplyr)



edgesdf %>%  filter( !(ego %in% nodes_df$ID)) %>%  pull(ego)

nodes_df$ID[14] == 


nodes_df <- nodes_df %>%  filter( (ID %in% unique(edgesdf$ego)) & (ID %in% unique(edgesdf$alter))  ) 

gg <- graph_from_data_frame(edgesdf, directed = TRUE, vertices = nodes_df)



library(ggplot2)
library(ggnetwork)

class(gg)

fortify(gg)

ggg <- ggnetwork(gg)
names(ggg)

ggplot(ggg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_nodes( shape ="")+
  geom_nodetext(aes(label=name))+
  geom_edges(aes(color =color )) +
  theme_blank()
