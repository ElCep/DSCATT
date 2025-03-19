library(ggplot2)
library(dplyr)
library(stringr)
library(hrbrthemes)
library(gridExtra)

calib_solutions <-  read.csv("~/DSCATT/model/openmole/results/realMeteoMedian.csv")


#paper solution is number 34 

papersolutionID <-  34


# extract lists of lists  in the single string 
dyns <- calib_solutions$populationDynamic[papersolutionID] %>% 
  str_remove("\\[\\[") %>% 
  str_remove("\\]\\]") %>% 
  str_split("\\],\\[") %>% 
  unlist() 

dynamic_to_vec <-  function(dyn){
return(dyn %>% str_split(",") %>% unlist() %>% as.numeric())
}

population <- dynamic_to_vec(dyns[1])
year <- 1995:2020
rainfall <- c(623,623,404,408,388,729,620,528,394,484,395,635,540,526,652    ,691,720,416,723,536,353,767,527,509,501,501)

pop_groundtruth <-  c(358  , 371  , 384  , 378  , 368  , 370  , 363  , 386  , 408  , 409  , 411  , 411  , 402    , 417  , 434  , 455  , 450  , 460  , 477  , 468  , 467  , 466  , 475  , 487  , 492  , 488  )



replicated_yields <- calib_solutions$milYieldDynamic[papersolutionID] %>%
  str_remove("\\[\\[") %>% 
  str_remove("\\]\\]") %>% 
  str_split("\\],\\[") %>% 
  unlist() 


milletYield <- replicated_yields[1]%>% str_split(",") %>% unlist() %>% as.numeric() 


replicated_fallowratios <- calib_solutions$effectiveFallowRatioDynamic[papersolutionID]%>%
  str_remove("\\[\\[") %>% 
  str_remove("\\]\\]") %>% 
  str_split("\\],\\[") %>% 
  unlist() 

fallowratio <-  replicated_fallowratios[1]%>% str_split(",") %>% unlist() %>% as.numeric() 




rainfallColor <-  "royalblue"
populationColor <-  "darkorange"
groundtruth_popColor <-  "darkolivegreen4"
milletYieldColor  <-  "chartreuse4"
fallowratioColor <-  "darkorchid"

yearly_dynamic_with_rainfall <-  data.frame(rainfall, year, population, pop_groundtruth, milletYield, fallowratio)



rainplot <-  ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=rainfall), color=rainfallColor)+
  geom_point(aes(y=rainfall), color=rainfallColor)+
  theme_ipsum(base_family = "Arial",) +
  theme(
    axis.title.y = element_text(color = rainfallColor, size=13),
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  labs(x="year", y="rainfall (mm)",title = "Diohine rainfall from 1995 to 2020")
rainplot  


groundtruth_pop_plot <-   ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=pop_groundtruth), color=groundtruth_popColor)+
  geom_point(aes(y=pop_groundtruth), color=groundtruth_popColor)+
  theme_ipsum(base_family = "Arial",) +
  theme(
    axis.title.y = element_text(color = groundtruth_popColor, size=13),
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  ylim(350,500)+
  labs(x="year", y="inhabitants ",title = "Sassem population from 1995 to 2020")
groundtruth_pop_plot  
  


dynplot <-  ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=population), color="darkorange")+
  theme_ipsum(base_family = "Arial",) +
  theme(
    axis.title.y = element_text(color = populationColor, size=13)
  )+
  labs(x="year", y="population")


yieldplot <-  ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=milletYield), color=milletYieldColor)+
  theme_ipsum(base_family = "Arial",) +
  theme(
    axis.title.y = element_text(color = milletYieldColor, size=13),
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  labs(x="year", y=parse(text='Yield  (kg.ha^(-1))'), title = "Annual millet yield" )
yieldplot



yield_rainfall_plot <-  ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=milletYield), color=milletYieldColor)+
  geom_line(aes(y=rainfall), color=rainfallColor)+
  theme_ipsum(base_family = "Arial",) +
  scale_y_continuous(
    
    # Features of the first axis
    #name = parse(text="Millet yield (kg.ha^-1)"),
    name = expression(paste("Millet yield ",(kg.ha^-1))),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="rainfall (mm)")
  )+
  theme(
    axis.title.y = element_text(color = milletYieldColor, size=13),
    axis.title.y.right = element_text(color = rainfallColor, size=13),
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  labs(x="year",  title = "Annuel millet yield vs. rainfall" )
yield_rainfall_plot


dev.off()

fallow_rainfall_plot <-  ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=fallowratio*500 + 300  ), color=fallowratioColor)+
  geom_line(aes(y=rainfall), color=rainfallColor)+
  theme_ipsum(base_family = "Arial",) +
  scale_y_continuous(
    
    # Features of the first axis
    #name = parse(text="Millet yield (kg.ha^-1)"),
    name = expression(paste("Fallow ratio ",(kg.ha^-1))),
    breaks = c(0,0.25,0.5,0.75,1)*500 + 300,
        labels=c("0", "0.25", "0.5" , "0.75", "1"),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1 , name="rainfall (mm)")
  )+
  theme(
    axis.title.y = element_text(color = fallowratioColor, size=13),
    axis.title.y.right = element_text(color = rainfallColor, size=13),
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  labs(x="year",  title = "Preserved fallow ratio vs. rainfall" )
fallow_rainfall_plot


pop_dyn_vs_groundtruth_pop_plot <-   ggplot(yearly_dynamic_with_rainfall, aes(x=year))+
  geom_line(aes(y=population), color=populationColor)+
  geom_point(aes(y=population), color=populationColor)+
  geom_line(aes(y=pop_groundtruth), color=groundtruth_popColor)+
  geom_point(aes(y=pop_groundtruth), color=groundtruth_popColor)+
  theme_ipsum(base_family = "Arial") +
  scale_y_continuous(
    sec.axis = dup_axis(name = "Simulated inhabitants")
  )+
  theme(
    axis.title.y = element_text(color = groundtruth_popColor, size=13),
    axis.title.y.right =  element_text(color = populationColor, size=13),
    
    axis.title.x = element_text(color = "grey20", size=13)
  )+
  labs(x="year", y="Groundtruth inhabitants",title = "Simulated  vs. groundtruth \n population from 1995 to 2020")
pop_dyn_vs_groundtruth_pop_plot





#phase diagrma


ggplot(yearly_dynamic_with_rainfall, aes(x=rainfall, y = population))+
  geom_path(color="chartreuse4")+
  geom_label(aes(label=year))+
  theme_ipsum(base_family = "Arial",) +
  theme(
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13)
  )+
  labs(x="rainfall", y="population")



