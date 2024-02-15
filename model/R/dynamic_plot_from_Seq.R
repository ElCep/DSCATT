library(ggplot2)
a <- c(0.8945794554018693, 0.9548909818816214, 1.0439115426292829, 1.0434778977694705, 1.0548124772087226, 1.0289732248722747, 1.0305283650591899, 1.0424311687975083, 1.0195078043115267, 1.0208236921619935, 1.0328461220685368, 1.0072261843738315, 1.0044398915389405, 1.0198866205109032, 0.9851402030654207, 0.9921283650591893, 1.015804377520249, 0.9721607638130837, 0.9826230691090337, 0.9868349071152649, 0.9638018853084112, 0.9539875550903419, 0.9391788323489103, 0.9014218230031147, 0.8899676173956386, 0.8489208884236761, 0.8136560909158879, 0.8042454990155755)
b<- c(0.7645794554018689, 0.8242928510404981, 1.0436722902928346, 1.0434778977694705, 1.0548124772087226, 1.0289732248722747, 1.0305283650591899, 1.0424311687975076, 1.019507804311527, 1.020823692161993, 1.0328461220685363, 1.0103414491713398, 1.0044398915389399, 1.0204847513520245, 0.9855389569595017, 0.9882654367102803, 1.0182965893582552, 0.9749021968348905, 0.9831215114766348, 0.9893520410716515, 0.9661595177071647, 0.9544959663052957, 0.9334467451214957, 0.8993532871775692, 0.8990791438753897, 0.8731701096074774, 0.8345557793894075, 0.8275975239376944)

df <- data.frame(value=a)
df$label <- "érosion positive"
df$id <- 1:nrow(df)

df2 <- data.frame(value=b)
df2$id <- 1:nrow(df2)
df2$label <- "érosion nulle"

df <- rbind(df, df2)
library(ggplot2)
ggplot(df, aes(x=id, y=value, color=label))+
  geom_line()+
  theme_light()

cor.test(a,b)


MilYield <- c(553.6505786605484, 562.6849066383995, 536.8982656787185, 579.6208502127988, 571.4527441884899, 554.7614842639757, 573.5504052610048, 576.8174924776761, 550.6073344542373, 572.0161649586281, 565.2301990622, 546.5575740661636, 548.1544530494341, 554.0105525727054, 549.1755907193564, 543.8061559930959, 546.444862045229, 539.4686829736314, 537.9536474162397, 546.2499214001949, 539.8800080770394, 534.939993868141, 532.923643584239, 538.2682263729627, 529.4048550588976, 540.1100940609047)
Population <- c(359, 366, 373, 380, 387, 394, 401, 409, 417, 425, 433, 441, 449, 457, 466, 472, 478, 479, 476, 479, 479, 475, 477, 478, 468, 473)
FallowRatio <- c(0.6601941747572816, 0.6448598130841121, 0.5833333333333334, 0.5728155339805825, 0.5327102803738317, 0.5092592592592593, 0.5048543689320388, 0.4205607476635514, 0.37962962962962965, 0.34951456310679613, 0.2897196261682243, 0.14814814814814814, 0.038834951456310676, 0.037383177570093455, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
HerdSize <-c(51, 49, 53, 50, 50, 51, 48, 49, 47, 46, 43, 38, 36, 36, 34, 33, 32, 29, 31, 30, 27, 30, 30, 26, 32, 31)

dyna <- data.frame(MilYield,Population,FallowRatio,HerdSize)
dyna$years <- 1:nrow(dyna) 




rendement_25ans <- ggplot(dyna, aes(x=years, y=MilYield))+
  geom_line()+
  xlab("années")+
  ylab("Rendement")+
  labs(title="Rendement du Mil", subtitle = "En kgs de grains par hectare")+
  scale_x_continuous(breaks=c(1,5,10, 15, 20 ,25))+
  theme_light()
rendement_25ans


library(scales)
jachere_25ans <- ggplot(dyna, aes(x=years, y=FallowRatio))+
  geom_line()+
  xlab("années")+
  ylab("jachère intacte")+
  labs(title="Proportion de jachère intacte")+
  scale_x_continuous(breaks=c(1,5,10, 15, 20 ,25))+
  scale_y_continuous(labels=percent)+
  theme_light()
jachere_25ans





population_25ans <- ggplot(dyna, aes(x=years, y=Population))+
  geom_line()+
  xlab("années")+
  ylab("habitants")+
  labs(title="Population du quartier")+
  scale_x_continuous(breaks=c(1,5,10, 15, 20 ,25))+
  theme_light()
population_25ans




library(reshape2)
melt_dyna <- melt(dyna,id.vars = "years")

quatre_en_1 <- ggplot(melt_dyna, aes(x= years, y = value, color=variable))+
  geom_line()+
  facet_wrap(~variable,ncol = 1,scales = "free")+
  theme_light()

quatre_en_1


# transformée de fourrier 
ffta <- fft(a)
plot(Mod(ffta))




##PSE display 
library(palmerpenguins)
library(plotly)
library(dplyr)


pse_diohine <- read.csv("~/tmp/PSE_diohine.csv")


pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.generation))
names(pse_diohine)



pp <- ggplot(pse_diohine, aes(label=giniParcels, label2=peanutSeedToFood, label3=mulching, label4=rotationCycle))+
  geom_point(aes(x=objective.om_lastSoilQuality, y=objective.om_lastEffectiveFallowRatio, color=rainFall, size=nbFaidherbia))+
  scale_size()+
  theme_light()
ggplotly(pp)





library(dplyr)
library(stringr)
library(scales)

calib <- read.csv("~/Téléchargements/calibration.csv")

calib <- calib %>%  filter(soilQualityBasis== 0.5312696251467559)


pop <- calib$populationDynamic
herd <- calib$herdDynamic
fallow <- calib$effectiveFallowRatioDynamic
foodStress <- c( 1.2003235642370162, 1.2106124665885027, 1.2033317631039386, 1.1520890134581578, 1.161365334765517, 1.1560631795716938, 1.101167930591851, 1.1092748180876761, 1.107756046481401, 1.06206777864579, 1.0658028161179558, 1.0610299643806296, 1.0395381675367958, 1.0447565970402204, 1.052041703104042, 1.0442141714830524, 1.026295760461376, 1.0285033256192166, 1.0223544426394755, 1.0171129646965842, 1.0246647143934124, 1.020711740685353, 1.0298020311500584, 1.0108954260880683, 1.021556772953544, 1.0193039699672912)



pop <- pop %>%  str_remove("\\[") %>% str_remove("\\]") %>%
  str_split(",") %>% unlist() %>% as.numeric() 
herd <- herd %>%  str_remove("\\[") %>% str_remove("\\]") %>%
  str_split(",") %>% unlist() %>% as.numeric() 
fallow <- fallow%>%  str_remove("\\[") %>% str_remove("\\]") %>%
  str_split(",") %>% unlist() %>% as.numeric() 

pop <- rescale(pop)
herd <- rescale(herd)
fallow <- rescale(fallow)



years<- 1:length(pop)
type<-rep("100% du troupeau", length(pop))




library(reshape2)
df_calib <- data.frame(years, population=pop, troupeau=herd, jachère=fallow, type, foodStress)
#df_calib <- data.frame(years,  fallow, type, foodStress)
melt_dyna <- reshape2::melt(df_calib,id.vars = c("years", "type"))

names(df_calib) %>% length()

ggplot(melt_dyna)+
  geom_line(aes(x=years, y=value, color=variable),lwd=1)+
  theme_light()


pop2 <-c( 359, 366, 373, 380, 387, 394, 401, 409, 417, 425, 433, 441, 447, 450, 455, 458, 462, 466, 465, 464, 465, 464, 466, 463, 464, 465)
herd2 <- c(145, 153, 156, 149, 156, 160, 152, 160, 166, 154, 156, 158, 145, 145, 146, 135, 135, 137, 128, 129, 130, 124, 129, 127, 126, 132)
fallow2 <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.9223300970873787, 0.897196261682243, 0.8611111111111112, 0.7475728155339806, 0.719626168224299, 0.7314814814814815, 0.6310679611650486, 0.5887850467289719, 0.6111111111111112, 0.5436893203883495, 0.5514018691588785, 0.5833333333333334, 0.5533980582524272, 0.5420560747663551, 0.5370370370370371, 0.5436893203883495, 0.5700934579439252)
foodStress2<- c(1.1700358193653548, 1.184506660439365, 1.1733599376465103, 1.1213543330703046, 1.125287836301775, 1.1245954582974922, 1.0777197947086024, 1.081980754347577, 1.0793500311610151, 1.05394593756082, 1.0524096068381161, 1.053415542976104, 1.0372004866904831, 1.030419296166409, 1.0282926520446707, 1.0276036331737943, 1.0238874026949565, 1.0238393355652113, 1.0173886455617704, 1.0066345887804002, 1.0224390654194153, 1.0113517832166585, 1.0250568210920037, 1.01904515315976, 1.0190984431646666, 1.0155638790336938)

pop2<- rescale(pop2)
herd2 <- rescale(herd2)
fallow2 <- rescale(fallow2)
type2<-rep("80% du troupeau", length(pop2))

df_calib2 <- data.frame(years, population=pop2, troupeau=herd2, jachère=fallow2, type=type2, foodStress=foodStress2)
#df_calib2 <- data.frame(years, fallow=fallow2, type=type2, foodStress=foodStress2)
melt_dyna2 <- melt(df_calib2,id.vars = c("years", "type"))

head(melt_dyna)
head(melt_dyna2)
final <- rbind(melt_dyna, melt_dyna2)

ggplot(final, aes(x=years, y=value, color=variable, linetype=type))+
  geom_line(lwd=1)+
  theme_light()+
  xlab("années")+
  ylab("valeur")


foodUBT <- read.table("~/Téléchargements/grassStandard.txt", header = F, sep="\n")


split_by_year <- function(foodUBT){
lili<-foodUBT %>%  t %>%  str_flatten()
lili <- lili %>%  str_split("YEAR") %>% unlist() 
lili %>%  length()
lili[1]
#premier élément vide
lili <- tail(lili, 26)
return(lili)
}

lili <- split_by_year(foodUBT)

sumMilGrass_by_year<-function(id_year){
one_year <- lili[id_year]

kitchensMF<- one_year %>% str_split("TOTAL") %>% unlist() 
nbKplusone <- kitchensMF %>% length
id_K_one_year <- 1:(nbKplusone - 1)

get_total_by_kitchen <- function(id){
kitchenTotal_id <- kitchensMF[id +1]%>%  str_split_i( pattern =  "M:", i=1) %>% str_remove(":") %>%  as.numeric()
return(kitchenTotal_id)
}

totaux_by_K_one_year <- lapply(id_K_one_year,get_total_by_kitchen) %>% unlist()
totaux_by_K_one_year



totaux_Mil_grass_by_K <-function(idK){
alim <- kitchensMF[idK]
mil_total <- 0
grass_total <- 0
repas_mil <- c()
repas_grass <-c()
if( str_detect(alim,  pattern="F:")){
#cat(" de la  fallow")
  
  repas <- alim %>%  str_split("F:") %>% unlist() 
  repas_grass <- tail(repas, length(repas)-1) %>% as.numeric()
  grass_total <- sum(repas_grass)
  alim_mil <- repas[1]
  repas_mil <- alim_mil %>% str_split(pattern = "M: ") %>%  unlist() 
  nb_repas <- length(repas_mil)
  repas_mil  <- repas_mil%>% tail(nb_repas -1) %>%   as.numeric()
  mil_total <- sum(repas_mil)
  }else{
 # cat("que du mil") 
  nb_repas <- length(alim)
  repas_mil <- alim %>% str_split(pattern = "M: ") %>%  unlist() %>% as.numeric()
  nb_repas <- length(repas_mil)
  mil_total <- sum(tail(repas_mil, nb_repas -1))
}
return(list(Mtot= mil_total, Ftot=grass_total))
}


oneyear_MF_byK<-sapply(id_K_one_year, totaux_Mil_grass_by_K) %>% t %>%  as.data.frame() 

oneyear_MF_byK$Mtot <- unlist(oneyear_MF_byK$Mtot)
oneyear_MF_byK$Ftot <- unlist(oneyear_MF_byK$Ftot)
oneyear_MF_byK$somme <- oneyear_MF_byK$Mtot + oneyear_MF_byK$Ftot
oneyear_MF_byK$totaux == totaux_by_K


sumMil <- sum(oneyear_MF_byK$Mtot)
sumGrass <- sum(oneyear_MF_byK$Ftot)
return(list(Mil= sumMil, Grass=sumGrass))
}
 

final_MilGrass_by_year <- sapply(1:26, sumMilGrass_by_year)  %>% t %>% as.data.frame()
final_MilGrass_by_year$Mil<- final_MilGrass_by_year$Mil %>%  unlist()
final_MilGrass_by_year$Grass<- final_MilGrass_by_year$Grass %>%  unlist()
final_MilGrass_by_year$year <- 1:26

pct_df <- data.frame(year=final_MilGrass_by_year$year)
pct_df$pctMil <- final_MilGrass_by_year$Mil / (final_MilGrass_by_year$Mil + final_MilGrass_by_year$Grass)
pct_df$pctGrass <- final_MilGrass_by_year$Grass / (final_MilGrass_by_year$Mil + final_MilGrass_by_year$Grass)





melt_pct <- melt(pct_df, id.vars="year")
ggplot(melt_pct, aes(x=year, y=value, fill= variable))+
  geom_area()+
  theme_light()



MilGrass_by_year <- reshape2::melt(final_MilGrass_by_year,id.vars="year")
MilGrass_by_year
ggplot(MilGrass_by_year, aes(x=year, y=value, color=variable ))+
  geom_area()+
  theme_light()




#
library(ggtern)

df <- rbind(df_calib, df_calib2)
names(df)
ggtern(df, aes(population, troupeau, jachère, color=type))+
  geom_path()+
  theme_showarrows() 


tutu <- read.table("~/tmp/mulching_80pct.csv", header = F, row.names = NULL, sep=",")                 
df<-t(tutu) %>% as.data.frame()
df %>%  class
names(df) <-df[1,]
df<- df[-1,]
df %>%  head()
summary(df)
write.csv(df,"~/tmp/mulching80pct.csv",row.names = F)


titi <- read.csv("~/tmp/mulching80pct.csv",header = T)
summary(titi)
