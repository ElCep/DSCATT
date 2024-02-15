library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

pse_diohine <- read.csv("./data/PSE_diohine.csv")

maxX <- max(pse_diohine$objective.om_lastEffectiveFallowRatio)
minX <- min(pse_diohine$objective.om_lastEffectiveFallowRatio)

maxY <- max(pse_diohine$objective.om_lastSoilQuality)
minY <- min(pse_diohine$objective.om_lastSoilQuality)

pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.generation))

library(stringr)

get_terminal_value<- function(ligne){
return(ligne %>% str_remove("\\[") %>% str_remove("\\]") %>%
  str_split(",") %>% unlist() %>% as.numeric() %>% tail(1))
}
  
get_mean_value<- function(ligne){
  return(ligne %>% str_remove("\\[") %>% str_remove("\\]") %>%
           str_split(",") %>% unlist() %>% as.numeric() %>% mean(, na.rm=T))
}



pse_diohine$milYieldDynamic  <- lapply(X = pse_diohine$milYieldDynamic, get_terminal_value) %>% unlist()
pse_diohine$migrantDynamic  <- lapply(X = pse_diohine$migrantDynamic, get_mean_value) %>% unlist()
pse_diohine$populationDynamic  <- lapply(X = pse_diohine$populationDynamic, get_terminal_value) %>% unlist()
pse_diohine$herdDynamic  <- lapply(X = pse_diohine$herdDynamic, get_mean_value) %>% unlist()
pse_diohine$kitchenSizeDynamic  <- lapply(X = pse_diohine$kitchenSizeDynamic, get_mean_value) %>% unlist()



  
  
ui <- fluidPage(
  titlePanel("PSE"),
  
  sidebarLayout(
    sidebarPanel(
   # Remplacez par vos options
    width = 2,
      
      
      sliderInput("rangeFaidherbia", 
                  label = "Nombre de Faidherbia:",
                  min = min(pse_diohine$nbFaidherbia), max = max(pse_diohine$nbFaidherbia), value = c(0, 25),sep = ""),
    sliderInput("peanutSeedToFood", "Ratio Riz Arachide",
                min = 1, max = 2.5, value = c(1, 2.5)),
    sliderInput("peanutForInexcess", "Arachide en excès",
                min = 0, max = 1, value = c(0, 1)),
    sliderInput("populationGrowth", "Croissance démographique",
                min = 0, max = 0.03, value = c(0, 0.03)),
    sliderInput("rainFall", "Pluviométrie",
                min = 200, max = 800, value = c(200, 800)),
    sliderInput("giniParcels", "Inégalité parcelles",
                min = 0.1, max = 0.5, value = c(0.1, 0.5)),
    sliderInput("mulching", "Paillage",
                min = 0, max = 1, value = c(0, 1))

    ),
    
        
    mainPanel(
      width = 10,
      fluidRow(
      plotOutput("plotPSE")),
        fluidRow(
          
          column(3,
          selectInput("loan", 
                      label = "Stratégie de prêt",
                      choices = c("AllExtraParcelsLoaner",
                                  "Selfish",
                                  "ExtraParcelsExceptFallowLoaner"),
                      selected = c("AllExtraParcelsLoaner",
                                   "Selfish",
                                   "ExtraParcelsExceptFallowLoaner"), multiple = T),
          
          selectInput("rotationCycle",
                      "Assolement :",
                      choices = c("ThreeYears", "TwoYears"),
                      selected=  c("ThreeYears", "TwoYears"), multiple = T), # Remplacez par vos options
          ),
          column(3,
          selectInput("ownFallowUse",
                      "Usage de sa jachère:",
                      choices = c("UseFallowIfNeeded", "NeverUseFallow"), 
                      selected=  c("UseFallowIfNeeded", "NeverUseFallow"), multiple = T), # Remplacez par vos options
          
          selectInput("foodDonation",
                      "Don de nourriture:",
                      choices = c("FoodForAllStrategy", "FoodForUsOnlyStrategy"),
                      selected= c("FoodForAllStrategy", "FoodForUsOnlyStrategy"), multiple = T), # Remplacez par vos options
          ),
          column(3,
          selectInput("drySeasonHerdStrategy",
                      "Pâturage saison sèche",
                      choices = c("OwnerOnly","AnywhereAnyTime","EverywhereByDayOwnerByNight"), 
                      selected= c("OwnerOnly","AnywhereAnyTime","EverywhereByDayOwnerByNight"), multiple = T
          ), # Remplacez par vos options
          selectInput("wetSeasonHerdStrategy",
                      "Pâturage Saison des pluie :",
                      choices = c("OwnerOnly","AnywhereAnyTime","EverywhereByDayOwnerByNight"), 
                      selected= c("OwnerOnly","AnywhereAnyTime","EverywhereByDayOwnerByNight"),
                      multiple = T) 
        ),
        column(3,
               selectInput("colorBy", "Choisir la Colonne pour la Couleur:",
                           choices = c("migrantDynamic",
                                       "populationDynamic",
                                       "herdDynamic", "milYieldDynamic",
                                       "kitchenSizeDynamic", 
                                       "loanStrategy",
                                       "ownFallowUse",
                                       "foodDonation",
                                       "drySeasonHerdStrategy",
                                       "wetSeasonHerdStrategy"),
                           selected="milYieldDynamic")
        )
    )
  )
)
)
server <- function(input, output) {
  
  pse_diohine_filtered <- reactive({
    pse_diohine_filtered <- pse_diohine[pse_diohine$nbFaidherbia   >= input$rangeFaidherbia[1] 
                                        & pse_diohine$nbFaidherbia <= input$rangeFaidherbia[2],]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$loanStrategy%in%input$loan,]
     pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$rotationCycle  %in% input$rotationCycle,]
     pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$ownFallowUse  %in% input$ownFallowUse,]
     pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$foodDonation  %in% input$foodDonation,]
     pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$drySeasonHerdStrategy %in% input$drySeasonHerdStrategy,]
     pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$wetSeasonHerdStrategy %in% input$wetSeasonHerdStrategy,]
  
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$peanutSeedToFood>= input$peanutSeedToFood[1] & pse_diohine_filtered$peanutSeedToFood<= input$peanutSeedToFood[2], ]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$peanutForInexcess>= input$peanutForInexcess[1] & pse_diohine_filtered$peanutForInexcess<= input$peanutForInexcess[2], ]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$populationGrowth>= input$populationGrowth[1] & pse_diohine_filtered$populationGrowth<= input$populationGrowth[2], ]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$rainFall>= input$rainFall[1] & pse_diohine_filtered$rainFall<= input$rainFall[2], ]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$giniParcels>= input$giniParcels[1] & pse_diohine_filtered$giniParcels<= input$giniParcels[2], ]
    pse_diohine_filtered <- pse_diohine_filtered[pse_diohine_filtered$mulching>= input$mulching[1] & pse_diohine_filtered$mulching<= input$mulching[2], ]
    
    
      })
  
  
  output$plotPSE <- renderPlot(
    ggplot(pse_diohine_filtered())+
      geom_point(aes(x=objective.om_lastSoilQuality, y=objective.om_lastEffectiveFallowRatio, color= pull(pse_diohine_filtered(),input$colorBy)), size=5 )+
       theme(axis.title.x = element_text(color = "grey20", size = 20),
             axis.text.y = element_text(color = "grey20", size = 13),
             axis.title.y = element_text(color = "grey20", size = 20))+ ylim(c(minX, maxX)) + xlim(minY, maxY)+
      #scale_color(name= element_blank())+
      scale_y_continuous(labels=percent)+
      xlab("Qualité du Sol")+
      ylab("Jachère préservée")
    )
  
}
shinyApp(ui, server)
