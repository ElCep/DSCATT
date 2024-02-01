library(dplyr)
library(ggplot2)


kitchens <-  read.csv("./DSCATT/model/R/kitchens.csv")




head(kitchens)
kitchens$KID <-as.factor(kitchens$KID)

ggplot(kitchens, aes(x=Year, y= Balance, color=KID))+
  geom_line()


names(kitchens)

ggplot(kitchens, aes(x=Year, y= N, color=KID))+
  geom_line()



kitchens$Year %>%  unique() %>% length()
