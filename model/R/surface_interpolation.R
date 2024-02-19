# Chargement des bibliothèques nécessaires pour l'analyse et la visualisation des données
library(shiny)                # Pour créer des applications web interactives
library(dplyr)                # Pour la manipulation de données (filtrage, sélection, etc.)
library(ggplot2)              # Pour la création de graphiques basés sur la grammaire des graphiques
library(GGally)               # Pour étendre les fonctionnalités de ggplot2, notamment pour les paires de graphiques
library(scales)               # Pour formater les échelles dans les graphiques ggplot2
library(plotly)               # Pour convertir les graphiques ggplot2 en graphiques interactifs
library(stringr)              # Pour la manipulation de chaînes de caractères

# Définition du répertoire de travail au dossier contenant le script R actuel
# Cela facilite l'accès aux fichiers relatifs à l'emplacement du script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Lecture des données depuis un fichier CSV et stockage dans 'pse_diohine'
pse_diohine <- read.csv("../data_simu//population14160.csv")

# Suppression de colonnes spécifiques non nécessaires pour l'analyse future
# Chaque ligne supprime une colonne différente du dataframe 'pse_diohine'
pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.generation))
pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.evaluated))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastEffectiveFallowRatio))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastPopulation))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastMilYield))

# Affichage des noms des colonnes du dataframe 'pse_diohine'
names(pse_diohine)

# Conversion des colonnes spécifiques de 'pse_diohine' en facteurs
# Cela est souvent nécessaire pour les visualisations catégorielles ou lors de l'utilisation de ces colonnes comme groupes ou facettes
pse_diohine$loanStrategy <- as.factor(pse_diohine$loanStrategy)
pse_diohine$wetSeasonHerdStrategy <- as.factor(pse_diohine$wetSeasonHerdStrategy)
pse_diohine$drySeasonHerdStrategy <- as.factor(pse_diohine$drySeasonHerdStrategy)
pse_diohine$foodDonation <- as.factor(pse_diohine$foodDonation)

pratiques <- c("ownFallowUse","loanStrategy", "foodDonation","drySeasonHerdStrategy", "wetSeasonHerdStrategy" )
last_val<- names(pse_diohine)[names(pse_diohine) %>% str_which("last")]
dynamics<- names(pse_diohine)[names(pse_diohine) %>% str_which("ynamic")]
objectives<-names(pse_diohine)[names(pse_diohine) %>% str_which("jective")] 
techniques <-c("peanutSeedToFood", "peanutForInexcess", "populationGrowth", "rainFall", "giniParcels", "mulching",  "nbFaidherbia")

pse_no_dyn <- pse_diohine %>% select(-c(dynamics))
names(pse_no_dyn)


## Filtre les données sur lonStrategie pour en faire 3 groupes
## faire l'interpolation du volume



## exemple 

library(deldir)
library(rgl)
# Create some fake data
x <- rnorm(100)
y <- rnorm(100)
z <- x^2 + y^2

# Triangulate it in x and y
del <- deldir(x, y, z = z)
triangs <- do.call(rbind, triang.list(del))

# Plot the resulting surface
plot3d(x, y, z, type = "n")
triangles3d(triangs[, c("x", "y", "z")], col = "gray")
rglwidget()


J'ai cette structure de donnée

df <- structure(list(peanutSeedToFood = c(1.41896783664886, 1.45277228739953, 
2.45130326046724, 1.05434906990347, 2.5, 1.18856873091444), rotationCycle = c("ThreeYears", 
"ThreeYears", "ThreeYears", "ThreeYears", "ThreeYears", "ThreeYears"
), ownFallowUse = c("UseFallowIfNeeded", "UseFallowIfNeeded", 
"UseFallowIfNeeded", "UseFallowIfNeeded", "UseFallowIfNeeded", 
"NeverUseFallow"), loanStrategy = structure(c(2L, 2L, 1L, 2L, 
1L, 1L), levels = c("AllExtraParcelsLoaner", "ExtraParcelsExceptFallowLoaner", 
"Selfish"), class = "factor"), foodDonation = structure(c(1L, 
1L, 1L, 2L, 1L, 2L), levels = c("FoodForAllStrategy", "FoodForUsOnlyStrategy"
), class = "factor"), drySeasonHerdStrategy = structure(c(3L, 
3L, 2L, 1L, 3L, 1L), levels = c("AnywhereAnyTime", "EverywhereByDayOwnerByNight", 
"OwnerOnly"), class = "factor"), wetSeasonHerdStrategy = structure(c(2L, 
1L, 2L, 1L, 3L, 3L), levels = c("AnywhereAnyTime", "EverywhereByDayOwnerByNight", 
"OwnerOnly"), class = "factor"), peanutForInexcess = c(0.204075483484078, 
0, 0, 0.980271967387735, 1, 0), populationGrowth = c(0.0216534214028414, 
0.0209127539511254, 0.0258451919817171, 0.023541493742164, 0.03, 
0.0269814394962919), rainFall = c(393L, 426L, 447L, 498L, 556L, 
793L), giniParcels = c(0.3, 0.5, 0.2, 0.5, 0.2, 0.1), mulching = c(0.697683482374631, 
0.791468432390147, 0.99934945670011, 0.544275598001969, 0.206438774619851, 
0.244827490081379), nbFaidherbia = c(2L, 22L, 6L, 8L, 12L, 12L
), objective.lastPopulation = c(358, 586, 660, 278, 568, 674), 
    objective.lastEffectiveFallowRatio = c(0.467289719626168, 
    0.242990654205607, 0.11214953271028, 0.953271028037383, 0.691588785046729, 
    0.383177570093458), objective.lastMilYield = c(439.690725265141, 
    775.814251408402, 679.97330320522, 813.522460805349, 902.411424780394, 
    1098.81893186616), kitchenSizeAverage = c(15.8757928697288, 
    16.2716104351704, 16.2941334106178, 14.7346970839618, 16.5862831689437, 
    16.3738636685857), populationRSquare = c(0.844470458063886, 
    0.994912755339121, 0.994514336829056, 0.643738668333855, 
    0.98789765744508, 0.994503423706156), populationSlope = c(2.18564102564102, 
    9.04444444444444, 11.9798290598291, 1.61230769230769, 8.47794871794872, 
    12.5347008547009)), row.names = c(NA, 6L), class = "data.frame")
    
Quand je plot avec plotly j'ai un nuage de point. Je veux visualiser les 3 volumes 
des enveloppe des 3 groupes dans R. 
