library(ggplot2)
library(sf)
library(dplyr)
#library(ggsflabel)
library(ggpubr)
library(ggnewscale)
library(RColorBrewer)

#simulated dynamics data 
parcelsdata <- read.csv("~/dev/DSCATT/model/R/data/parcels.csv")
parcelsdata %>%  head


parcelsGeometry <-  st_read("~/dev/DSCATT/model/R/data/parcelMap.gpkg")
# useless column removal
parcelsGeometry <- parcelsGeometry %>% select(-c("idParcelNeighborhood"))
parcelsGeometry$ownerID <-  as.factor(parcelsGeometry$ownerID) 
#attributes
summary(parcelsGeometry)
# change region ID with label Zone
parcelsGeometry$regionID <- paste0("Zone ",parcelsGeometry$regionID)


# nb parcels and total surface by kitchen 
aggregStatesByK <-  parcelsGeometry %>% group_by(ownerID) %>% summarise(nb_pcls = n(), sum_surf= sum(area)/10000) %>% st_drop_geometry() %>% select(nb_pcls,sum_surf)



nb_parcels_histogramm <- ggplot(aggregStatesByK, aes(x=nb_pcls))+
  geom_histogram(fill="darkcyan", color="lightgray",bins = 10)+
  labs(title = "Kitchens number of parcels")+
  xlab(label = "number of parcels")+
  theme_light()
nb_parcels_histogramm


srface_histogramm <- ggplot(aggregStatesByK, aes(x=sum_surf))+
  geom_histogram(fill="royalblue", color="lightgray",bins = 8)+
  labs(title = "Kitchens arable land area")+
  xlab(label = "Arable surface (ha)")+
  theme_light()
srface_histogramm







centro_zone1 <-  parcelsGeometry %>% filter(regionID=="Zone 1") %>%  st_union() %>%  st_centroid()
centro_zone2 <-  parcelsGeometry %>% filter(regionID=="Zone 2") %>%  st_union() %>%  st_centroid()
centro_zone3 <-  parcelsGeometry %>% filter(regionID=="Zone 3") %>%  st_union() %>%  st_centroid()

labels_zones <- st_sf(label=unique(parcelsGeometry$regionID),geom=c(centro_zone1,centro_zone2, centro_zone3))


carto_parcel_byK <-  ggplot(parcelsGeometry)+
  geom_sf( aes(fill=ownerID))+
  geom_sf_label(data = labels_zones, aes(label=label), size=3)+
  scale_fill_discrete(name= "Kitchen ID")+
  labs(title= "Simulated village territory", subtitle = paste0(length(unique(parcelsGeometry$ownerID))," Kitchens"))+
  theme(legend.position=c("bottom"),
        legend.box = "vertical",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks =  element_blank(),
        axis.title = element_blank())+
   guides(fill = guide_legend(nrow = 2))
carto_parcel_byK


combined_graph <- ggarrange(carto_parcel_byK, ggarrange(nb_parcels_histogramm, srface_histogramm,  nrow=3) , ncol=2, widths = c(3, 1))
combined_graph



global_min_QS <-  min(parcelsdata$QS)
global_max_QS <-  max(parcelsdata$QS)
QSlimits <- c(global_min_QS, global_max_QS)




year_map <-  function(année, parcelsGeometry, parcelsdata, maxPtsize=3) {
  ayear <-
    inner_join(parcelsGeometry,
               parcelsdata %>% filter(Year == année),
               by = join_by(parcelID == ID))
  
  ayear$QSscaled <-
    (ayear$QS - min(ayear$QS)) / (max(ayear$QS) - min(ayear$QS))
  
  parcels_centroids <-
    st_centroid(ayear) %>% st_geometry() %>% st_coordinates() %>% as.data.frame()
  ayear$centroidX <- parcels_centroids$X
  ayear$centroidY <- parcels_centroids$Y
  p2 <-  ggplot(ayear,) +
    #  geom_sf(data = parcelsGeometry$geom, color="grey")+
    geom_sf(color = "gray", aes(fill = for.crop, alpha = QS),) +
    scale_fill_viridis_d(name = "Cropping") +
    scale_alpha(name = "Soil Quality",range = c(0.5,1),guide="none") +
    #add a new legend slot
    new_scale_color() +
    geom_point(aes(
      x = centroidX,
      y = centroidY,
      color = Manure.ha,
      size = Manure.ha
    )) +
    scale_color_distiller(palette = "Oranges",
                          direction = 1,
                          guide = "legend") +
    scale_size_area(max_size = maxPtsize) +
    labs(title = paste("Year ", unique(ayear$Year))) +
     theme(legend.position=c("right"),
           legend.box = "vertical",
           panel.background = element_blank(),
           axis.text = element_blank(),
           axis.ticks =  element_blank(),
           axis.title = element_blank())
    # guides(fill = guide_legend(nrow = 2))
  
  
  return(p2)
}



maxPointSize <- 1
years <- unique(parcelsdata$Year) %>% order()

graph_list <- lapply(years, year_map, parcelsGeometry, parcelsdata,maxPointSize)



three_years_map <- ggarrange(plotlist = graph_list, common.legend = T, nrow=2, ncol=5)
three_years_map















library(gganimate)


st_crs(full) <- NA_crs_
p2 <-  ggplot(full, aes(fill=for.crop))+
  geom_sf(color="gray")+
  scale_fill_viridis_d()+
  theme_void()
p2






p2 <- p2+ transition_states(Year, state_length = 2, transition_length = 2)+enter_manual() +
  exit_manual()+ labs(title = "Année {closest_state}")
p2





my.cols <- function(n) {
  black <- "#000000"
  if (n <= 9) {
    c(black,brewer.pal(n-1, "Set2"))
  } else {
    c(black,hcl(h=seq(0,(n-2)/(n-1),
                      length=n-1)*360,c=100,l=65,fixup=TRUE))
  }
}

library(ggplot2)
d <- data.frame(z=1:10)
g1 <- ggplot(data=d,aes(x=z,fill=factor(z)))+
  geom_bar()

g1 + scale_colour_manual(values=my.cols(10))
## check that we successfully recreated ggplot2 internals
## g1+scale_colour_discrete()




#real village 


sassem <-  st_read("~/tmp/FichiersSIG_Sob_Bary_Sassem/Parcellaire Sassem/Parcellaire carte des cate╠ügories de champs.shp")
sassem <-  st_read("~/dev/DSCATT/model/R/data/Parcellaire carte des cate╠ügories de champs.shp")

sassem$N._FOYER <-  as.factor(sassem$N._FOYER)
plot(sassem["N._FOYER"])


st_write(sassem, dsn="~/DSCATT/model/R/data/sassem_field_categories.gpkg", delete_dsn = T, delete_layer = T)



# nb parcels and total surface by kitchen 
aggreg_foyer <-  sassem %>% group_by(N._FOYER) %>% summarise(nb_pcls = n(), sum_surf= sum(SUPERFICIE)/10000 , n_kad=sum(Kadd)) %>% st_drop_geometry() %>% select(nb_pcls,sum_surf, N._FOYER)



nb_parcels_histogramm <- ggplot(aggreg_foyer, aes(x=nb_pcls))+
  geom_histogram(fill="darkcyan", color="lightgray",bins = 15)+
  labs(title = "Kitchens number of parcels")+
  xlab(label = "number of parcels")+
  theme_light()
nb_parcels_histogramm

nb_faidherbia_histogramm <- ggplot(aggreg_foyer, aes(x=nb_pcls))+
  geom_histogram(fill="chartreuse4", color="lightgray",bins = 15)+
  labs(title = "Kitchens Faidherbia trees")+
  xlab(label = "total number of Faidherbias")+
  theme_light()
nb_faidherbia_histogramm


srface_histogramm <- ggplot(aggreg_foyer, aes(x=sum_surf))+
  geom_histogram(fill="royalblue", color="lightgray",bins = 8)+
  labs(title = "Kitchens arable land area")+
  xlab(label = "Arable surface (ha)")+
  theme_light()
srface_histogramm




carto_sassem_byK <-  ggplot(sassem)+
  geom_sf( aes(fill=N._FOYER))+
  #geom_sf_label(data = labels_zones, aes(label=label), size=3)+
  scale_fill_discrete(name= "Foyer")+
  labs(title= "Sassem", subtitle = paste0(length(unique(sassem$N._FOYER))," cuisines"))+
  theme(legend.position=c("bottom"),
        legend.box = "vertical",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks =  element_blank(),
        axis.title = element_blank())+
  guides(fill = guide_legend(nrow = 3))
carto_sassem_byK

combined_graph <- ggarrange(carto_sassem_byK, ggarrange(nb_parcels_histogramm, srface_histogramm, nb_faidherbia_histogramm,nrow=3) , ncol=2, widths = c(3, 1))
combined_graph


library(DescTools)

Gini(aggreg_foyer$nb_pcls)
Gini(aggreg_foyer$sum_surf)

Gini(c(10, rep(5,5), rep(1,4)))




aggreg_tri <- aggreg_foyer %>% arrange(nb_pcls) %>%   mutate(N._FOYER=factor(N._FOYER, levels=N._FOYER))
aggreg_tri$order <- 1:nrow(aggreg_tri)

ggplot(aggreg_tri, aes(x= order, y=nb_pcls))+
  geom_point()+
  #geom_smooth(formula = y ~ x)+
  theme_minimal()+
  scale_y_discrete(limits=as.character(1:18))+
  ylab("nombre d'items")+
  xlab("rang du client")


aggreg_foyer <-  sassem %>% filter (Categorie_ == 2) %>%  group_by(N._FOYER) %>% summarise(nb_pcls = n(), sum_surf= sum(SUPERFICIE)/10000 , n_kad=sum(Kadd)) %>% st_drop_geometry() %>% select(nb_pcls,sum_surf, N._FOYER)









sassem %>%  filter(Categorie_ == 2) %>%  st_area() %>%  sum




