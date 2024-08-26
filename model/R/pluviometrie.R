library(climenv)
library(sf)
library(dplyr)
library(terra)
library(stringr)
#  path to the polygons of the zone 
parcels_path <- "/home/PChapron/DSCATT/model/R/data/sassem_light.gpkg"



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







remote_files_url <- "~/Téléchargements/envidatS3paths.txt" %>% file.path()
urls<- read.delim(remote_files_url, header = F)
urls_missing <- read.delim("~/Téléchargements/envidatS3paths_FULL.txt", header = F)
past_years <- seq(from=1979, to=1994) %>%  as.character()

pluvio <-  data.frame(month=character(), year=character(), mean=numeric())



#CAUTION
#Very long loop
# each iteration take ~5 minutes of downloading 


for (y in past_years){
  cat(y)
  cat("\n############\n")
  year_URLS <- urls_missing %>%  filter(str_detect(V1,y)) %>% pull

  for( i in 1:length(year_URLS)){
    cat("########################\n")
    cat(i, "/",length(year_URLS),"in", y, "\n" )
    cat("########################\n")
  uu <- year_URLS[i]%>% trim
  fifi_temp <- tempfile()
  download.file(uu ,destfile = fifi_temp, method="wget",quiet = F)

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
  } #month loop
  
    write.csv(pluvio,paste0("pluviometrieSassem_",y,".csv"))
}




# for( i in 1:25){
#   cat("########################\n")
#   cat(i, "/ 25\n" )
#   cat("########################\n")
# uu <- urls$V1[i]  
# fifi_temp <- tempfile()
# download.file(uu,destfile = fifi_temp, method="wget",quiet = F)
# 
# #convert file to raster object 
# r <-  rast(fifi_temp)
# 
# # extract raster pixel values over parcels
# parcels_ctrds_coord <-  st_centroid(parcels) %>% st_coordinates() %>% as.data.frame()
# names(parcels_ctrds_coord) <- c("x","y")
# values_over_sassem <- terra::extract(r, parcels_ctrds_coord)
# 
# 
# #get month and year from filename
# filename <- uu %>% str_split("/") %>% unlist() %>% last()
# month <- filename %>% str_extract("\\d\\d")
# year <- filename %>% str_extract("\\d\\d\\d\\d")
# 
# precip <-  select(values_over_sassem,starts_with("file")) %>% pull
# 
# 
# # add a new line to the dataframe
# one_month <-  list(month, year, mean(precip,na.rm=T))
# names(one_month) <-  names(pluvio)
# pluvio <- bind_rows(pluvio, one_month)
# cat("fichier du" , month, "de ", year, "donne une moyenne de " ,one_month$mean ,"\n")
# #release the temporary file 
# unlink(fifi_temp, recursive=T)
# }
# 
# 
# write.csv(pluvio, "~/DSCATT/model/R/pluviometrieSassem.csv")



##gathering csv files


csvFiles <- list.files("~/DSCATT/model/R/", pattern = '^pluviometrieSassem*',full.names = T)

file_into_df <- function(file){
 return(read.csv(file) %>% select(c(month, year, mean)))
}
result_list <- lapply(csvFiles, file_into_df)
allmonths <- do.call(rbind, result_list)
allmonths$mean <- allmonths$mean / 100


allmonths$month <- as.factor(allmonths$month)
levels(allmonths$month) <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#remove possible duplicate
allmonths <- allmonths %>%  distinct(month, year, mean)

allmonths %>%  group_by(month) %>%  summarise(n())


allmonths %>%  filter(month =="Nov")


ggplot(allmonths, aes(x=month, y=mean, group=year))+
  #geom_point(aes(size=mean))+
  geom_line(aes(color=year))+
  labs(title = "precipitation over Sassem from 1995 to 2019")+
xlab("month")+
  ylab("monthly mean precipitation (mm)")+
  theme_light()


yearly <-  allmonths %>%  group_by(year) %>%  summarise(yearly_precip= sum(mean))

vecvec <- yearly %>% filter(year >=1995 ) %>% filter(year <=2019 ) %>% pull(yearly_precip)


write.csv(yearly, "~/DSCATT/model/R/pluviometrie_annuelle_1979_2019.csv")


# 2019 is not complete
yearly <- yearly %>% filter(year<2019)
yearly$year
ggplot(yearly, aes(fill=yearly_precip))+
  geom_col(aes(x=year, y= yearly_precip ), color="darkgray",lwd=0.1)+
  scale_fill_distiller(palette="Blues", direction=1 ,name = "yearly \nrainfall\n(mm)")+
  ylab("yearly mean precipitation (mm)")+
  labs(title = paste0("Precipitation over Sassem from ",min(yearly$year)," to ",max(yearly$year)))+
  theme_light()


rnorm(2, mean= 510, sd= 120)

yearly %>% filter(year!=1983) %>% pull(yearly_precip) %>% sd

