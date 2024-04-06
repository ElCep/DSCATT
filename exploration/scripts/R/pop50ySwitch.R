library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

# rf->700
rf700Pop<- c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 488, 494, 500, 506, 513, 520, 526, 525, 521, 502, 500, 498, 473, 479, 482, 472, 475, 476, 473, 475, 477, 471, 472, 475, 476)

basePop = c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 488, 494, 500, 503, 497, 491, 473, 472, 472, 457, 458, 464, 445, 450, 455, 440, 446, 450, 446, 446, 452, 446, 444, 450, 439)

fd6Pop = c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 488, 494, 500, 506, 513, 520, 527, 534, 541, 548, 555, 562, 569, 576, 583, 591, 599, 607, 615, 620, 621, 622, 622, 619, 619)

selfishPop = c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 351, 330, 306, 295, 294, 295, 297, 295, 298, 302, 303, 301, 303, 303, 301, 302, 304, 301, 300, 304, 305, 305, 307, 303, 303)

twoYearsPop = c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 488, 494, 500, 506, 499, 502, 473, 479, 452, 458, 438, 444, 428, 433, 429, 434, 433, 438, 426, 431, 425, 430, 425, 430, 425)

grazingAAPop = c(356, 360, 364, 369, 374, 379, 384, 389, 394, 399, 404, 409, 414, 419, 424, 429, 434, 440, 446, 452, 458, 464, 470, 476, 482, 488, 494, 500, 506, 513, 520, 527, 534, 541, 548, 555, 562, 564, 565, 562, 557, 550, 545, 538, 533, 533, 527, 523, 522, 519)

#dataframe distribué par année 
#qsk3g1,qsk3,qsk3g3,qsk3g4,
#qsk2g1, qsk2, qsk2g3, qsk2g4
df2 <- data.frame(rf700Pop)
df2$years <- 1:nrow(df2)
df2 <- melt(df2, id.vars ="years")
df2$type <- "RF=700, t=25"


df3 <- data.frame(basePop)
df3$years <- 1:nrow(df3)
df3 <- melt(df3, id.vars ="years")
df3$type <- "Base"

df4 <- data.frame(fd6Pop)
df4$years <- 1:nrow(df4)
df4 <- melt(df4, id.vars ="years")
df4$type <- "FD=6, t=25"

df5 <- data.frame(selfishPop)
df5$years <- 1:nrow(df5)
df5 <- melt(df5, id.vars ="years")
df5$type <- "No Loan, t=25"

df6 <- data.frame(twoYearsPop)
df6$years <- 1:nrow(df6)
df6 <- melt(df6, id.vars ="years")
df6$type <- "Two years, t=25"


df7 <- data.frame(grazingAAPop)
df7$years <- 1:nrow(df7)
df7 <- melt(df7, id.vars ="years")
df7$type <- "Grazing Anywhere, anytime"


dfTot = rbind(df2,df4, df5, df6, df7, df3)

ggplot(dfTot, aes(x=years))+
  geom_line( aes(y=value, color=variable))+
  theme_minimal()+
  xlab("Years")+
  ylab("Population")+
  geom_vline(xintercept=25, linetype='dotted', col = 'black')+
  scale_color_manual(name = "Switch, Years = 25", 
                     values=c( "blue", "darkgreen","#D95F02", "purple", "#1B9E77", "red" ),
                     labels=c("Rainfall = 700mm", "6 Faidherbia/ha","No Loan, no food donation", "2 years rotation", "Grazing anywhere anytime", "Base"))
