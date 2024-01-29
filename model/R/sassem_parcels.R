library(sf)
library(dplyr)
library(ggpubr)

setwd("~/DSCATT/model/R/")

sassem <-  st_read("~/tmp/FichiersSIG_Sob_Bary_Sassem/Parcellaire Sassem/Parcellaire carte des cate╠ügories de champs.shp")

# 
# sassem$UTL_2012 <- as.factor(sassem$UTL_2012)
# sassem$ZONE_CARTO <- as.factor(sassem$ZONE_CARTO)
# plot(sassem["UTL_2012"])
# unique(sassem$SAISON)


sob <-  st_read("~/tmp/FichiersSIG_Sob_Bary_Sassem/ParcellaireSOb/ParcellaireSob_versionTheoThibaultsAvantDerniere.shp" )


# unique(sob$Age_Jacher)
# 
# airetot <-  st_area(sob) %>% sum
# sob %>%  filter(Age_Jacher ==3) %>% st_area() %>% sum / airetot
# 
sassem <- sassem %>%  select(N._PARCELL, N._FOYER, NOM_UTILIS,NOM_PROPRI, NOM_CHAMPS, NOMBR_ARBR, TYP_SOL, Kadd, Categorie_,geometry)
st_write(sassem, "./data/sassem_light.gpkg", delete_layer = T)


#anonymisation des noms


# names to factors conversion . leveles taken from union of possible values
noms_distincts <- union(sassem$NOM_PROPRI, sassem$NOM_UTILIS) %>% unique() 
sassem$NOM_PROPRI <- factor(sassem$NOM_PROPRI , levels=noms_distincts)
sassem$NOM_UTILIS <- factor(sassem$NOM_UTILIS , levels=noms_distincts)

# shuffle two letters labels 
codes <- outer(LETTERS, LETTERS, paste0) %>% as.vector()
codes_shuffle <- sample(codes, size=length(codes),replace = F)

# two letters labels affected as former names for owner and user
sassem$owner <-  codes_shuffle[as.integer(sassem$NOM_PROPRI)]
sassem$User <-  codes_shuffle[as.integer(sassem$NOM_UTILIS)]                  



#plot(sassem["Categorie_"])


champs_brousse_sassem <-  sassem %>%  filter(Categorie_==2) 


sassem$KID <-  as.factor(sassem$N._FOYER)
sob$KID<- as.factor(sob$Num_Foyer_)


sob$Surface_Ca %>% as.numeric()
plot(sassem["N._FOYER"])

st_write(sassem, "~/DSCATT/model/R/data/sassem_field_categories.gpkg")


library(stringr)
sob$Surface_Ca <-  str_replace(sob$Surface_Ca,",",".")
sob$Surface_Ca <-  as.numeric(sob$Surface_Ca)
sob$geometry <-  st_make_valid(sob$geometry)

sob <-  sob[!is.na(sob$Surface_C),] 

# nb parcels and total surface by kitchen 
aggreg_foyer_sob <-  sob %>% group_by(KID) %>% summarise(nb_pcls = n(), sum_surf= sum(Surface_Ca)) %>% st_drop_geometry() %>% select(nb_pcls,sum_surf, KID)




aggreg_foyer_sassem <-  sassem %>% group_by(KID) %>% summarise(nb_pcls = n(), sum_surf= sum(SUPERFICIE)/10000 ) %>% st_drop_geometry() %>% select(nb_pcls,sum_surf, KID)




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




aggreg_foyer_sassem$Village <- "Sassem"
aggreg_foyer_sob$Village <- "Sob"

aggreg_foyer_sassem <-  aggreg_foyer_sassem %>%  arrange(nb_pcls) %>%   mutate(KID=factor(KID, levels=KID))

aggreg_foyer_sob <-  aggreg_foyer_sob %>%  arrange(nb_pcls) %>%   mutate(KID=factor(KID, levels=KID))


deux_villages <-  rbind(aggreg_foyer_sassem, aggreg_foyer_sob)



aggreg_foyer_sassem %>% anyNA()
aggreg_foyer_sob %>% anyNA()


cumul_surf_sassem <- cumsum(aggreg_foyer_sassem$sum_surf)
cumul_surf_sob <-  cumsum(aggreg_foyer_sob$sum_surf)
dfsob <-  data.frame(cum_surf=cumul_surf_sob, Village="SOB")
dfsassem <-  data.frame(cum_surf=cumul_surf_sassem, Village="SASSEM")
dfsob$iidd <-  1: nrow(dfsob)
dfsassem$iidd <-  1: nrow(dfsassem)
deuxvillages <- rbind(dfsob, dfsassem)

head(deuxvillages)

aggreg_tri <- aggreg_foyer %>% arrange(nb_pcls) %>%   mutate(Num_Foyer_=factor(Num_Foyer_, levels=Num_Foyer_))
aggreg_tri$order <- 1:nrow(aggreg_tri)

ggplot(deuxvillages, aes(y= cum_surf, color=Village, x=iidd))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()+
  labs(title = "Logarithme des surfaces cumulées des parcelles de Sob et Sassem", subtitle = "agregation par cuisine")
# scale_y_discrete(limits=as.character(1:20))
#  ylab("number of parcels")+
# xlab("Kitchen ID")




