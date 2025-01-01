## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
## library(sarima) ## simulate: sim_arima()
## library(forecast)
## library(fracdiff)




########
#### varve
########

(nn <- length(varve))
plot(varve)
lambda <- -1/2
plot( (varve^lambda - 1)/lambda)
lV <- log(varve)
plot(lV, main="log(varve)", ylab="" )
acf2(lV)
acf2(diff(lV))
lag1.plot(diff(lV), 6)


## fit (could use stats::arima
(fit1V <- astsa::sarima(lV, p=0, d=1, q=1, no.constant=F))
acf2(resid(fit1V$fit))
fit2V <- astsa::sarima(lV, p=1,d=1,q=1)
fit3V <- astsa::sarima(lV, p=0,d=1,q=2)

##IC's?
fit1V$BIC * nn
fit2V$BIC * nn
fit3V$BIC * nn

fit1V$AICc * nn
fit2V$AICc * nn
fit3V$AICc * nn

fit1V$ttable ## 0,1
fit2V$ttable ## 1,1
plot(ARMAacf(ar=.234, ma=-.887, lag.max=8), type="h")
plot(ARMAacf(ar=.234, ma=-.887, lag.max=8, pacf=T), type="h")

## forecast
## (can try different n.ahead values)
sarima.for(lV, n.ahead=50, p=1, d=1, q=1, no.constant=F)
##sarima.for(lV, n.ahead=100, p=1, d=0,q=1, no.constant=F)  ##compare





