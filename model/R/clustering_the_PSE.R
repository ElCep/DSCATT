library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(stringr)
library(GGally)


pse_diohine <- read.csv("~/Téléchargements/population14160.csv")
pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.generation))
pse_diohine <- pse_diohine %>% dplyr::select(-c(evolution.evaluated))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastEffectiveFallowRatio))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastPopulation))
pse_diohine <- pse_diohine %>% dplyr::select(-c(lastMilYield))


maxX <- max(pse_diohine$objective.lastEffectiveFallowRatio)
minX <- min(pse_diohine$objective.lastEffectiveFallowRatio)

maxY <- max(pse_diohine$objective.lastMilYield)
minY <- min(pse_diohine$objective.lastMilYield)

names(pse_diohine)

pse_diohine$loanStrategy <- as.factor(pse_diohine$loanStrategy)
pse_diohine$wetSeasonHerdStrategy <- as.factor(pse_diohine$wetSeasonHerdStrategy)
pse_diohine$drySeasonHerdStrategy <- as.factor(pse_diohine$drySeasonHerdStrategy)
pse_diohine$foodDonation <- as.factor(pse_diohine$foodDonation)
pse_diohine$ownFallowUse <- as.factor(pse_diohine$ownFallowUse)





pratiques <- c("ownFallowUse","loanStrategy", "foodDonation","drySeasonHerdStrategy", "wetSeasonHerdStrategy" )
last_val<- names(pse_diohine)[names(pse_diohine) %>% str_which("last")]
dynamics<- names(pse_diohine)[names(pse_diohine) %>% str_which("ynamic")]
objectives<-names(pse_diohine)[names(pse_diohine) %>% str_which("jective")] 
techniques <-c("peanutSeedToFood", "peanutForInexcess", "populationGrowth", "rainFall", "giniParcels", "mulching",  "nbFaidherbia"
) 


pse_no_dyn <- pse_diohine %>% select(-c(dynamics))
names(pse_no_dyn)
#ggpairs(pse_no_dyn)


pse_num <- pse_diohine %>%  select(where(is.numeric))
names(pse_num)
#ggpairs(pse_num)


mat_cor<- cor(pse_num) 
diag(mat_cor) <- NA
mat_cor[upper.tri(mat_cor)]<- NA


top_15_cor <- sort(abs(mat_cor),decreasing = T) %>% head(15) 

coords<-which(mat_cor >  last(top_15_cor),arr.ind = T)
couples_top15 <- c()
for (i in 1:nrow(coords)){
  couples_top15 <- c(couples_top15, mat_cor[coords[i,1], coords[i,2],drop=F] %>% dimnames() %>%unlist() %>% as.vector())
}
couples_top15 <- unique((couples_top15))
couples_top15

top_15_correl_variables <- pse_diohine %>%  select(all_of(couples_top15))
ggpairs(top_15_correl_variables)
names_top15<-names(top_15_correl_variables)

top15_et_pratique <-cbind(top_15_correl_variables, pse_diohine %>% select(all_of(pratiques)))
ggpairs(top15_et_pratique, columns = names_top15, aes(color=loanStrategy), size=0.02)




#c'est la loan strategy qui capte le + de variance du rendement de mil final
# toutes les tratégies sont significatives sauf la wetrSeasonHerdStrategy
aov(pse_diohine$objective.lastMilYield~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% summary.aov() 

aov(pse_diohine$objective.lastPopulation~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% summary.aov() 

aov(pse_diohine$objective.lastEffectiveFallowRatio~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% summary.aov() 


df_obj <- select(pse_diohine,all_of(objectives))
df_prat <- select(pse_diohine,all_of(pratiques))

objectives_plus_pratiques <- cbind(df_obj,df_prat )

ggpairs(objectives_plus_pratiques, aes(color=loanStrategy), columns = objectives, title = "Effet du prêt", )
ggpairs(objectives_plus_pratiques, aes(color=ownFallowUse), columns = objectives, title= "Effet ownFallow")
ggpairs(objectives_plus_pratiques, aes(color=foodDonation), columns = objectives, title= "Effet don")
#bof
ggpairs(objectives_plus_pratiques, aes(color=drySeasonHerdStrategy), columns = objectives, title = "Effet dry paturage")
ggpairs(objectives_plus_pratiques, aes(color=wetSeasonHerdStrategy), columns = objectives,, title = "Effet wet paturage")

library(randomForest)


RF_pse <- pse_diohine %>%  select(all_of(techniques))
RF_pse <- cbind(RF_pse, pse_diohine$objective.lastMilYield)
names(RF_pse)[which(names(RF_pse)=="pse_diohine$objective.lastMilYield")] <-"lastMilYield"
set.seed(55)
indivs <- sample(1:2, nrow(RF_pse), replace = TRUE, prob = c(0.7, 0.3))
train <- RF_pse[indivs==1,]
test <- RF_pse[indivs==2,]


train %>% head()


rf1 <- randomForest(lastMilYield~.,
                    data=train, proximity=TRUE, ntree=2000,
                    importance=T, mtry=5) 


plot(rf1)

#ntree 500 c'est bien 
rf1 <- randomForest(lastMilYield~.,
                    data=train, proximity=TRUE, ntree=500,
                    importance=T, mtry=5) 

importance(rf1)
varImpPlot(rf1,           sort = T,
           main = "Variable Importance regarding Mil yield")

summary(rf1)



RF_pse <- pse_diohine %>%  select(all_of(techniques))
RF_pse <- cbind(RF_pse, pse_diohine$objective.lastEffectiveFallowRatio)
names(RF_pse)[which(names(RF_pse)=="pse_diohine$objective.lastEffectiveFallowRatio")] <-"lastEffectiveFallowRatio"
set.seed(55)
indivs <- sample(1:2, nrow(RF_pse), replace = TRUE, prob = c(0.7, 0.3))
train <- RF_pse[indivs==1,]
test <- RF_pse[indivs==2,]





rf2 <- randomForest(lastEffectiveFallowRatio~.,
                    data=train, proximity=TRUE, ntree=500,
                    importance=T, mtry=5) 


plot(rf2)
importance(rf2)
varImpPlot(rf2,           sort = T,
           main = "Variable Importance regarding  lastEffectiveFallowRatio")


RF_pse <- pse_diohine %>%  select(all_of(techniques))
RF_pse <- cbind(RF_pse, pse_diohine$objective.lastPopulation)
names(RF_pse)[which(names(RF_pse)=="pse_diohine$objective.lastPopulation")] <-"lastPopulation"
set.seed(55)
indivs <- sample(1:2, nrow(RF_pse), replace = TRUE, prob = c(0.7, 0.3))
train <- RF_pse[indivs==1,]
test <- RF_pse[indivs==2,]


rf3 <- randomForest(lastPopulation~.,
                    data=train, proximity=TRUE, ntree=500,
                    importance=T, mtry=5) 


plot(rf3)
importance(rf3)
varImpPlot(rf3,           sort = T,
           main = "Variable Importance regarding  lastPopulation")



library(tidymodels)
data_split <- initial_split(pse_no_dyn, strata = "foodDonation")
data_train <- training(data_split)
data_test <- testing(data_split)

rf_recipe <- 
  recipe(formula = foodDonation ~ ., data = data_train) %>%
  step_zv(all_predictors())

## feature importance sore to TRUE, and the proximity matrix to TRUE
rf_spec <- rand_forest() %>%
  set_engine("randomForest", importance = TRUE, proximity = TRUE) %>%
  set_mode("classification")

rf_workflow <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec)


rf_fit <- fit(rf_workflow, data = data_train)
proximity_mat<- rf_fit$fit$fit$fit$proximity
dim(proximity_mat)
rownames(proximity_mat)<- data_train[, 5]
colnames(proximity_mat)<- data_train[, 5]
# turn it to a distance 
pse_distance<- dist(1-proximity_mat)

# hierarchical clustering
hc<- hclust(pse_distance)

# cut the tree to 3 clusters
clusters<- cutree(hc, k = 3)

as.dendrogram(hc) %>%
  plot()

length(clusters)
pse_diohine %>%  group_by(loanStrategy) %>% summarise(n())

#nuage 3D
library(plotly)
plyply<- plot_ly(pse_no_dyn, x=~objective.lastPopulation, 
                 y=~objective.lastMilYield,
                 z=~objective.lastEffectiveFallowRatio, 
                 color=~loanStrategy, size=~rainFall^2)
plyply <- plyply %>%  add_markers()
plyply




#nomenclature solidarités

pratiques

aov(pse_diohine$objective.lastMilYield+pse_diohine$objective.lastPopulation~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% anova()


pse_diohine$conjonction_objectifs <- rescale(pse_diohine$objective.lastMilYield)* rescale(pse_diohine$objective.lastPopulation) * pse_diohine$objective.lastEffectiveFallowRatio
pse_diohine$disjonction_objectifs <- rescale(pse_diohine$objective.lastMilYield)+ rescale(pse_diohine$objective.lastPopulation) + pse_diohine$objective.lastEffectiveFallowRatio
hist(pse_diohine$conjonction_objectifs)
hist(pse_diohine$disjonction_objectifs)

filter(pse_diohine, conjonction_objectifs > 0.7)


aov(pse_diohine$disjonction_objectifs~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% anova()
aov(pse_diohine$conjonction_objectifs~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% anova()


aov(pse_diohine$objective.lastPopulation~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% anova()

aov(pse_diohine$objective.lastEffectiveFallowRatio~ pse_diohine$loanStrategy + pse_diohine$ownFallowUse + pse_diohine$foodDonation + pse_diohine$drySeasonHerdStrategy +pse_diohine$wetSeasonHerdStrategy ) %>% anova()



pratiques


score_loan_strat<- function(loanStrategy){
  switch (loanStrategy %>% as.character(),
        AllExtraParcelsLoaner = 3,
        ExtraParcelsExceptFallowLoaner = 2,
        Selfish = 0
)
}

score_ownF_strat<- function(ownFallowUse){
  switch (ownFallowUse %>% as.character(),
          UseFallowIfNeeded = 1, #pas zero car si culture , possibilité de don de nourriture 
          NeverUseFallow = 3
  )
}

score_Donation_strat<- function(foodDonation){
  switch (foodDonation %>% as.character(),
          FoodForUsOnlyStrategy = 0,
          FoodForAllStrategy = 2
  )
}


score_dryHerd_strat<- function(drySeasonHerdStrategy){
  switch (drySeasonHerdStrategy %>% as.character(),
          OwnerOnly = 0,
          EverywhereByDayOwnerByNight = 2,
          AnywhereAnyTime = 3
  )
}

score_wetHerd_strat<- function(wetSeasonHerdStrategy){
  switch (wetSeasonHerdStrategy %>% as.character(),
          OwnerOnly = 0,
          EverywhereByDayOwnerByNight = 2,
          AnywhereAnyTime = 3
  )
}





scores_loans <-lapply(pse_diohine$loanStrategy, score_loan_strat) %>% unlist()
scores_ownF <-lapply(pse_diohine$ownFallowUse, score_ownF_strat) %>% unlist()
scores_Donation <-lapply(pse_diohine$foodDonation, score_Donation_strat) %>% unlist()
scores_dryHerd <-lapply(pse_diohine$drySeasonHerdStrategy, score_dryHerd_strat) %>% unlist()
scores_wetHerd <-lapply(pse_diohine$wetSeasonHerdStrategy, score_wetHerd_strat) %>% unlist()

pse_diohine$solidarity <- scores_Donation+ scores_dryHerd+ scores_loans+ scores_ownF+ scores_wetHerd

library(plotly)
plyply<- plot_ly(pse_no_dyn, x=~objective.lastPopulation, 
                 y=~objective.lastMilYield,
                 z=~objective.lastEffectiveFallowRatio, 
                 color=~solidarity, size=0.1)
plyply <- plyply %>%  add_markers()
plyply



ggplot(pse_diohine)+
  geom_point(aes(x=objective.lastPopulation, y=objective.lastEffectiveFallowRatio, color= solidarity, size=nbFaidherbia))
  
ggplot(pse_diohine)+
  geom_point(aes(x=objective.lastMilYield, y=objective.lastEffectiveFallowRatio, color= solidarity, size=nbFaidherbia))

ggplot(pse_diohine)+
  geom_point(aes(x=objective.lastMilYield, y=objective.lastPopulation, color= solidarity, size=nbFaidherbia))+theme_light()

