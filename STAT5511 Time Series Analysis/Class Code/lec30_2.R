## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
## library(sarima) ## simulate: sim_arima()
##library(forecast)

## source("class/lec30_2_dat.R")
source("class/lec30_2_dat-2.R")

## analysis
## xx
plot(xx)
invisible(acf2(xx))


## re-run with different models:
ignore <- capture.output(fit1 <- astsa::sarima(xx, p=2,0,q=2))
fit1$BIC * nn
acf2(fit1$fit$residuals)

##
library(moments)
skewness(fit1$fit$residuals)
skewness(xx) ## (although this could be related to time series structure)
plot(xx, type="p")
mean(xx); abline(h=mean(xx))

































##
lam <- 1/3
yy <- (xx^lam - 1)/lam
## yy <- log(xx)
plot(yy)

acf2(yy)

## re:run w diff models:
ign <- capture.output(fit11 <- astsa::sarima(yy, p=1, 0, 1))
ign <- capture.output(fit21 <- astsa::sarima(yy, p=2, 0, 1))
ign <- capture.output(fit12 <- astsa::sarima(yy, p=1, 0, 2))
ign <- capture.output(fit22 <- astsa::sarima(yy, p=2, 0, 2))

fit11$BIC * nn
fit21$BIC * nn
fit12$BIC * nn
fit22$BIC * nn

## acf2(fit2$fit$residuals)
