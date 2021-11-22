library(DiagrammeR)
library(readr)
library(igraph)
library(stringr)
library(dplyr)
library(visNetwork)



setwd("~/DSCATT/PARDi/")

lignes_du_dot <-  read_lines("./diagram_pardi_simple_edges.dot")
source("./code_R/parsing_dot.R")

obj_result <- generate_dataframe_and_graph(lignes_du_dot)


nodes_df <-  obj_result$nodes
edges_df <-obj_result$edges
gg <- obj_result$graph


#extraction des sous graphes
edges_interac <-  edges_df %>%  filter(pardi_type=="interaction")
nodes_interac <- nodes_df %>%  filter(name %in% union(edges_interac$ego, edges_interac$alter))
gg_interac <- graph_from_data_frame(edges_interac, directed = TRUE, vertices = nodes_interac)

plot.igraph(x = gg_interac, 
            layout= layout_with_kk(gg_interac),
            edge.arrow.size = 0.2,
            edge.label = NA    )


edges_conflit <-  edges_df %>%  filter(pardi_type=="dynamique_conflit")
nodes_conflit <- nodes_df %>%  filter(name %in% union(edges_conflit$ego, edges_conflit$alter))
gg_conflit <- graph_from_data_frame(edges_conflit, directed = TRUE, vertices = nodes_conflit)

plot.igraph(x = gg_conflit, 
            layout= layout_with_kk(gg_conflit),
            edge.arrow.size = 0.2,
            edge.label=NA)


edges_fertilite <-  edges_df %>%  filter(pardi_type=="dynamique_fertilite")
nodes_fertilite <- nodes_df %>%  filter(name %in% union(edges_fertilite$ego, edges_fertilite$alter))
gg_fertilite <- graph_from_data_frame(edges_fertilite, directed = TRUE, vertices = nodes_fertilite)

plot.igraph(x = gg_fertilite, 
            layout= layout_with_kk(gg_fertilite),
            edge.arrow.size = 0.5)



#vizu avec diagrameR

#nodes
nn <- create_node_df(nrow(nodes_interac), 
                     label = nodes_interac$name, 
                     type=nodes_interac$type,
                     shape  = ifelse(nodes_interac$type== "acteur", "ellipse", "box"),
                     width = (nodes_interac$name) %>% as.character() %>%  nchar / 10,
                     color= ifelse(nodes_interac$type== "acteur", "CornflowerBlue", "DarkTurquoise"), 
                     #color="darkgrey",
                     fillcolor= ifelse(nodes_interac$type== "acteur", "CornflowerBlue", "DarkTurquoise") 
                     )

# edges
ee <- create_edge_df(from = match(as.character(edges_interac$ego) , as.character(nn$label)),
                     to= match(as.character(edges_interac$alter) , as.character(nn$label)), 
                     rel = edges_interac$label,
                     color= "grey")

#directed graph
dig_gg <- create_graph(graph_name = "interactions",
              nodes_df = nn, 
             edges_df = ee,
             directed = T
                )

#rendu interactif
render_graph(dig_gg, output = "visNetwork" ) %>% 
  visNodes(physics = F ) 




