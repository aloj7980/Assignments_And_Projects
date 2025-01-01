## Charles R Doss

## 2020 Did not do this in class.

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

plot(gnp)
acf2(gnp)
lgnp <- log(gnp)
plot(lgnp)
acf2(lgnp)
gnpgr <- diff(lgnp)  ## growth rate
plot(gnpgr)
acf2(gnpgr)
sarima(gnpgr, 1, 0, 0)
sarima(gnpgr, 0, 0, 2)

r102 <- sarima(gnpgr, 1, 0, 2)

r112 <- sarima(lgnp, 1, 1, 2)



## alternative "growth rate" series.
gnpgr2 <- diff(gnp) / gnp[1:(length(gnp)-1)]
plot(gnpgr2)
acf2(gnpgr2)
