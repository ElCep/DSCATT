library(DiagrammeR)
library(readr)
library(igraph)
library(stringr)
library(dplyr)
library(visNetwork)


# Set working folder to the one where this script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#lignes_du_dot <-  read_lines("./diagram_pardi_simple_edges.dot")
source("./parsing_dot.R")
lignes_du_dot <-  read_lines("../diagram_simplified_soil_pardi_simple_edges.dot")

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



########################################
## graphes agriculteurs et eleveurs
##########################################"
# L'objectif est de produire automatiquement le graphe qui mets en relation avec Eleveur et/ou agriculteur


# filtrage des noeuds on se concentre sur eleveur et agriculteur
gg_agro_pasteur <- subgraph.edges(gg, E(gg)[inc(V(gg)[name=c("Eleveur","Agriculteur")])]) 

#Filtrage des liens 
# on enlève les dynamique qui auront une autre visualisation, on ne garde que les liens d'interactions
gg_agro_pasteur2 <-subgraph.edges(gg_agro_pasteur, E(gg_agro_pasteur)[pardi_type=="interaction"])




plot.igraph(x = gg_agro_pasteur2, 
            layout= layout_with_kk(gg_agro_pasteur2),
            edge.arrow.size = 0.5)






# Variante 
nodes_agroPasteur <-  nodes_df %>%  filter (name %in% c("Eleveur","Agriculteur") ) 

voisins_agropasteur_ordre2 <-  make_ego_graph(graph = gg,
              order = 1,
              nodes = V(gg)[name  %in% c("Eleveur","Agriculteur")],
              mode = "all",
              mindist = 1)



voisins_agropasteur_ordre2[[1]] %>% plot.igraph()



edf <- voisins_agropasteur_ordre2[[1]] %>% get.edgelist() %>%  as.data.frame()
names(edf) <-  c("from", "to")

ndf <-   data.frame(name=V(voisins_agropasteur_ordre2[[1]])$name, id= V(voisins_agropasteur_ordre2[[1]])$ID)

nn <- create_node_df(nrow(ndf), 
                     label = ndf$name, 
                     
)



ee <- create_edge_df(from = match(as.character(edf$from) , as.character(nn$label)),
                     to= match(as.character(edf$to) , as.character(nn$label)), 
                                        color= "grey")

ee


dig_gg <- create_graph(graph_name = "interactions",
                       nodes_df = nn,
                       edges_df = ee ,
                       directed = T
)

#rendu interactif
render_graph(dig_gg, output = "visNetwork" ) %>% 
  visNodes(physics = F ) 





library(visNetwork)
library(htmlwidgets)
E(gg_agro_pasteur2)$color <- "grey"
V(gg_agro_pasteur2)$color <- "blue"
saveWidget(visIgraph(gg_agro_pasteur2, layout = "layout_nicely"), file = "./test.html")




#vizu avec diagrameR

vizu_interactive <- function(df_nodes, df_edges){
nn <- create_node_df(nrow(df_nodes), 
                     label = df_nodes$name, 
                     type=df_nodes$type,
                     shape  = ifelse(df_nodes$type== "acteur", "ellipse", "box"),
                     width = (df_nodes$name) %>% as.character() %>%  nchar / 10 ,
                     #color= ifelse(df_nodes$type== "acteur", "black", "gray80"), 
                     color= ifelse(df_nodes$type== "acteur", "CornflowerBlue", "DarkTurquoise") 
                     #color="darkgrey",
                     #fillcolor= ifelse(df_nodes$type== "acteur", "CornflowerBlue", "DarkTurquoise") 
                     #fillcolor= ifelse(df_nodes$type== "acteur", "black", "gray80") 
                     #fontcolor="pink"
                     )

# edges
ee <- create_edge_df(from = match(as.character(df_edges$ego) , as.character(nn$label)),
                     to= match(as.character(df_edges$alter) , as.character(nn$label)), 
                     rel = df_edges$label
                     #color= "grey"
                     )

#directed graph
dig_gg <- create_graph(graph_name = "interactions",
              nodes_df = nn, 
             edges_df = ee,
             directed = T
                )

#rendu interactif
render_graph(dig_gg, output = "visNetwork" ) %>% 
  visNodes(physics = F ) 
return(dig_gg)
}

I

vizu_interactive(nodes_interac,edges_interac )
vizu_interactive(nodes_conflit,edges_conflit )
vizu_interactive(nodes_fertilite,edges_fertilite )





### conversion de l'objet igraph vers deux dataframe pour la fonction vizu_interactive


nodes_agri_elev <-  igraph::as_data_frame(gg_agro_pasteur2, what="vertices")
edges_agri_elev <-  igraph::as_data_frame(gg_agro_pasteur2, what="edges")
names(edges_agri_elev)[1] <- "ego"
names(edges_agri_elev)[2] <- "alter"

grph_to_display <- vizu_interactive(nodes_agri_elev, edges_agri_elev)

grph_to_display <- grph_to_display %>% render_graph(output = "visNetwork" ) 
grph_to_display
grph_to_display %>%  visNodes(physics = F ,
           font= list(color= "white")
           ) %>% 
  visEdges()


# render_graph(grph_to_display, output = "visNetwork" ) %>% 
#   visNodes(physics = F ) 
# 
# visNodes(render_graph(grph_to_display,output = "visNetwork"), physics = F)
# 
# 
# render_graph(grph_to_display) 


## TODO

# 1 : 
# toutes les relations d'ordre 1 qui concernent agri et eleveur 
# toutes les relation d'ordre deux de ces deux noeuds là 
# + les liens entre les voisins d'ordre et les voisins d'ordre 2 

#2: 
# trouver les boucles de rétroactions 


# question générale : en s'appuyant sur 
## objectif : trouver le groupe d'acteurs/ressources dans lequel il y a un lien 
#  soit direct , soit boucle , soit indirect 
# pour faire apparaître un groupe solidaire 





