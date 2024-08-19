library(climenv)
library(sf)
library(dplyr)
library(terra)
library(stringr)
#  path to the polygons of the zone 
parcels_path <- "/home/PChapron/DSCATT/model/R/data/sassem_light.gpkg"


# temp file to store climatic data 
temp_path <- tempfile()
dir.create(file.path(temp_path, "prec"), recursive = TRUE)

dir.create(file.path(temp_path, "/elev"), recursive = TRUE)
dir.create(file.path(temp_path, "/prec"), recursive = TRUE)
dir.create(file.path(temp_path, "/tmax"), recursive = TRUE)
dir.create(file.path(temp_path, "/tavg"), recursive = TRUE)
dir.create(file.path(temp_path, "/tmin"), recursive = TRUE)

list.dirs(temp_path)

# zone + reprojection en WGS84
parcels <- st_read(parcels_path)
parcels <- st_transform(parcels, 4326)

parcels<- parcels[-280,]
parcels <- parcels %>% rename("ID"= "N._PARCELL")

parcels$pluvio <- NA


##############CITATION ##############




# CHELSA_cmip5_ts
# 
# Karger, D.N., Schmatz, D., Detttling, D., Zimmermann, N.E. (2020): High resolution monthly precipitation and temperature timeseries for the period 2006-2100. Scientific Data. https://doi.org/10.1038/s41597-020-00587-y



#CHELSA_cmip5_ts (together with peer reviewed citation: CHELSA_cmip5_ts)

#Karger, D.N., Schmatz, D., Dettling, G., Zimmermann, N.E. (2019): High resolution monthly precipitation and temperature timeseries for the period 2006-2100. EnviDat. http://dx.doi.org/doi:10.16904/envidat.124




#get the files from chelsa servers 
#create some tif files for each month
#datachelsa <- chelsa(output_dir = file.path(temp_path), var = "prec", quiet = FALSE)

# 
# data_pluvio <- ce_download(
#   output_dir=file.path(temp_path),
#   var = "prec",
#   location=parcels,
#   c_source = "CHELSA",
# 
#   )
# # 
# pluvio_Sassem<- ce_extract(
#   path=  file.path(temp_path),
#   location = parcels,
#   c_source = "CHELSEA",
#   location_g = "ID",
#   var= "prec"
# )
# # 





#RASTER FILE READING 



tif_dir <- "~/DSCATT/model/R/data/data_rainFall_CHELSA_1981_2010/"

remote_files_url <- "~/Téléchargements/envidatS3paths.txt" %>% file.path()
urls<- read.delim(remote_files_url, header = F)




#dataframe to store results every month every year 

if(file.exists("~/DSCATT/model/R/pluviometrieSassem.csv")){
  pluvio <- read.csv("~/DSCATT/model/R/pluviometrieSassem.csv")
  pluvio$month <- as.character(pluvio$month)
  pluvio$year <- as.character(pluvio$year)
}else{pluvio <-  data.frame(month=character(), year=character(), mean=numeric())}

for( i in 151:180){
  cat("########################\n")
  cat(i, "/", ,"\n" )
  cat("########################\n")
uu <- urls$V1[i]  
fifi_temp <- tempfile()
download.file(uu,destfile = fifi_temp, method="wget",quiet = T)

#convert file to raster object 
r <-  rast(fifi_temp)

# extract raster pixel values over parcels
parcels_ctrds_coord <-  st_centroid(parcels) %>% st_coordinates() %>% as.data.frame()
names(parcels_ctrds_coord) <- c("x","y")
values_over_sassem <- terra::extract(r, parcels_ctrds_coord)


#get month and year from filename
filename <- uu %>% str_split("/") %>% unlist() %>% last()
month <- filename %>% str_extract("\\d\\d")
year <- filename %>% str_extract("\\d\\d\\d\\d")

precip <-  select(values_over_sassem,starts_with("file")) %>% pull


# add a new line to the dataframe
one_month <-  list(month, year, mean(precip,na.rm=T))
names(one_month) <-  names(pluvio)
pluvio <- bind_rows(pluvio, one_month)
cat("fichier du" , month, "de ", year, "donne une moyenne de " ,one_month$mean ,"\n")
#release the temporary file 
unlink(fifi_temp, recursive=T)
}


write.csv(pluvio, "~/DSCATT/model/R/pluviometrieSassem.csv")

# tif_files_names <- list.files(tif_dir,full.names = T)
# fifi <- tif_files_names[1] %>% file.path()
# r <- rast(fifi)
# plot(r)

