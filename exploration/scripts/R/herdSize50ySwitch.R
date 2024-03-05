library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

# rf->700
rf700HS<- c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 187, 170, 175, 171, 161, 162, 156, 148, 147, 137, 135, 139, 130, 131, 137, 127, 125, 135, 128, 127, 135, 126, 128, 137, 126)

baseHS = c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 171, 154, 153, 145, 135, 139, 129, 123, 131, 120, 121, 126, 113, 119, 121, 113, 116, 120, 112, 116, 120, 111, 117, 122, 109)

fd6HS = c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 190, 180, 188, 199, 188, 192, 200, 195, 191, 199, 193, 194, 201, 197, 199, 201, 195, 196, 194, 190, 191, 188, 186, 188, 189)

selfishHS = c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 142, 145, 147, 145, 148, 152, 150, 153, 153, 153, 154, 154, 153, 155, 155, 155, 155, 154, 154, 156, 154, 155, 155, 153, 153)

twoYearsHS = c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 176, 121, 177, 123, 160, 114, 146, 106, 133, 95, 119, 89, 116, 84, 116, 82, 116, 87, 110, 76, 110, 74, 109, 74, 109)

grazingAAHS = c(193, 204, 186, 195, 203, 184, 197, 202, 191, 197, 209, 191, 202, 211, 193, 199, 207, 188, 193, 201, 182, 181, 189, 173, 168, 171, 166, 173, 181, 171, 171, 181, 173, 176, 182, 171, 175, 173, 162, 166, 162, 155, 161, 154, 146, 152, 146, 144, 150, 142)

#dataframe distribué par année 
#qsk3g1,qsk3,qsk3g3,qsk3g4,
#qsk2g1, qsk2, qsk2g3, qsk2g4
df2 <- data.frame(rf700HS)
df2$years <- 1:nrow(df2)
df2 <- melt(df2, id.vars ="years")
df2$type <- "RF=700, t=25"


df3 <- data.frame(baseHS)
df3$years <- 1:nrow(df3)
df3 <- melt(df3, id.vars ="years")
df3$type <- "Base"

df4 <- data.frame(fd6HS)
df4$years <- 1:nrow(df4)
df4 <- melt(df4, id.vars ="years")
df4$type <- "FD=6, t=25"

df5 <- data.frame(selfishHS)
df5$years <- 1:nrow(df5)
df5 <- melt(df5, id.vars ="years")
df5$type <- "No Loan, t=25"

df6 <- data.frame(twoYearsHS)
df6$years <- 1:nrow(df6)
df6 <- melt(df6, id.vars ="years")
df6$type <- "Two years, t=25"


df7 <- data.frame(grazingAAHS)
df7$years <- 1:nrow(df7)
df7 <- melt(df7, id.vars ="years")
df7$type <- "Grazing Anywhere, anytime"

dfTot = rbind(df2,df4, df5, df6, df7, df3)

ggplot(dfTot, aes(x=years))+
  geom_line( aes(y=value, color=variable))+
  theme_minimal()+
  xlab("Years")+
  ylab("Herd size")+
  geom_vline(xintercept=25, linetype='dotted', col = 'black')+
  scale_color_manual(name = "Switch, Years = 25", 
                     values=c( "blue", "darkgreen","#D95F02", "purple", "#1B9E77", "red" ),
                     labels=c("Rainfall = 700mm", "6 Faidherbia/ha","No Loan, no food donation", "2 years rotation", "Grazing anywhere anytime", "Base"))
