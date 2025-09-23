


Navail <- runif(10000, min=0, max=100)

nmil <- sapply(0.501 * log (Navail) - 1.2179, min, 1)
NRFmil <- sapply(nmil,max,0.25)
nfallow <-  sapply(0.501 * log (Navail) - 1.2179, min, 1)
NRFfallow <- sapply(nfallow, max,0.25)




dfmil <-  data.frame(N=Navail, NRF=NRFmil, cultivar="Millet")
dffallow <-  data.frame(N=Navail, NRF=NRFfallow, cultivar="Fallow")
df <-  rbind(dfmil, dffallow)

library(ggplot2)
ggplot(df, aes(x=N, y=NRF, color=cultivar))+
  geom_point()+
  theme_light()

