# Charger le package
library(qrcode)

setwd("~/Téléchargements/")

code <- qr_code("HELLO WORLD")
plot(code)
