## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
library(sarima) ## simulate: sim_arima()
library(forecast)

source("class/lec31-2-dat.R")

## analysis
## xx
plot(xx)
acf2(xx)

## re-run with different models:
fit1 <- astsa::sarima(xx, p=1,0,q=1)
acf2(fit1$fit$residuals)

##
library(moments)
skewness(fit1$fit$residuals)
skewness(xx) ## (although this could be related to time series structure)
plot(xx, type="p")
mean(xx); abline(h=mean(xx))
































##
lam <- 1/4
yy <- (xx^lam - 1)/lam

## re:run w diff models:
fit2 <- astsa::sarima(yy, p=1,0,1)
acf2(fit2$fit$residuals)
