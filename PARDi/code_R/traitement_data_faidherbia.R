library(sf)
library(dplyr)
library(units)




 #faidherbia <-  read_sf("~/tmp/data_faidherbia/FA_Individual_Trees_NASA_Resize_RAMSES2_Prediction.shp")
# small_faiherbia <-  select(faidherbia,fid, Lat, Long, Area,Faidherbia, geometry)
# st_write(small_faiherbia,"~/tmp/faidherbia_redux.shp")


faidherbia <-  st_read("~/tmp/faidherbia_redux_SHP_v1/faidherbia_redux.shp")
faidherbia <-  faidherbia %>%  filter(Faidherbia == 1)
names(faidherbia)
faidherbia <- select(faidherbia,Lat, Long, geometry, Area)
#st_write(faidherbia,"~/tmp/fafileidherbia_only_redux.shp")



faidherbia <-  st_read("~/tmp/faidherbia_only_redux/faidherbia_only_redux.shp")
bboxFA <-  st_as_sfc(st_bbox(faidherbia))
centro_FA <-  st_centroid(faidherbia)
rm(faidherbia)




#densité globale 
aire_zone_ha <- bboxFA %>% st_area() 
units(aire_zone_ha) <-  "ha"
aire_zone_ha
density_global <- nrow(centro_FA) /aire_zone_ha

#densité au metre carré
nrow(centro_FA) / ( bboxFA %>% st_area())


st_crs(centro_FA)

# grille carré 1 carré : 100 ha de surface , 1000m de côté 


# cellsize c'est l'arète du carré 
grigri <- st_make_grid(bboxFA, cellsize = 1000) 
plot(grigri)




nb_FA_par_km2 <- lengths(st_intersects(grigri, centro_FA))
grigri <-  st_as_sf(grigri)
grigri$nb_FA <- nb_FA_par_km2

plot(grigri)

hist(nb_FA_par_km2,breaks = 30)


library(leaflet)

spl10000 <-  sample_n(centro_FA, 10000)


map_leaflet <- leaflet(spl10000 %>% st_transform(4326)) %>%
  addTiles() %>% 
  addCircleMarkers(weight = 1, fillOpacity = 0, radius = 3) 
  
map_leaflet

library(htmlwidgets)
saveWidget(map_leaflet, file="~/tmp/faidherbia.html")



