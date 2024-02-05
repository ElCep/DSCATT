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



