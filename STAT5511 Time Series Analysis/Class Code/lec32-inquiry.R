## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
library(sarima) ## simulate: sim_arima()
library(forecast)


pdf("lec32-inquiry.pdf") ##

## look at AirPassengers data: LBP p-values strange trends?
(fit1AP <- astsa::sarima(log(AirPassengers), p=0, d=1,q=1, P=0, D=1, Q=1, S=12))


## trends replicated by somewhat heavy tailed data
set.seed(10)

nn <- 144
mod <- list(nseasons=12, iorder=1, siorder=1, ma=-.4, sma=-.56)
xx <- sim_sarima(model=mod, n=nn,
                 rand.gen=function(n){rt(n=n,df=5)})
(fitxx <- astsa::sarima(xx, p=0,d=1,q=1, P=0,D=1,Q=1, S=12))

dev.off()
