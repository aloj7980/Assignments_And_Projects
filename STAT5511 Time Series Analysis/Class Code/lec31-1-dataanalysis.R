## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
library(sarima) ## simulate: sim_arima()
library(forecast)

## library(fracdiff)



########
## seasonal component
########


PHI1 <- .8
##PHI1 <- 0
## PHI1 <- 0 ## compare
phi <- c(rep(0,11), PHI1)
## phi1 <- -.6
## phi <- c(phi1, rep(0,10), PHI1, -phi1*PHI1)
theta <- -.5
## theta <- 0
ACF <- ARMAacf(ar=phi, ma=theta, 100)[-1]
PACF <- ARMAacf(ar=phi, ma=theta, 100, pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim=c(-.4, .8))
abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-.4, .8))
abline(h=0)

plot(xx <- arima.sim(model=list(ar=phi, ma=theta), n=288))




########
## seasonal nonstationarity
########

nn <- 500
phi <- .8
theta <- -.5
xx <- sarima::sim_sarima(model=list(sar=phi, sma=theta, siorder=1,
                            nseasons=10), n=nn)
acf2(xx, max.lag=70)
## acf2(diff(xx), max.lag=70)
acf2(diff(xx, lag=10), max.lag=70)

## Can examine the IC's
(res1 <- astsa::sarima(xx, p=0,d=0,q=0, P=1,D=1,Q=0, S=10))
(res1 <- astsa::sarima(xx, p=0,d=0,q=0, P=1,D=1,Q=1, S=10))


 ## Note: d+D>1: no intercept by default;
(res1 <- astsa::sarima(xx, p=0,d=1,q=0, P=1,D=1,Q=0, S=10))
## in other packages (e.g., sarima::sarima) may also need to check





########
#### varve
########

nn <- length(varve)
plot(varve)
lambda <- 1/2
plot( (varve^lambda - 1)/lambda)
lV <- log(varve)
plot(lV, main="log(varve)", ylab="" )
acf2(lV)
acf2(diff(lV))
lag1.plot(diff(lV), 6)


## fit (could use stats::arima
(fit1V <- astsa::sarima(lV, p=0,d=1, q=1, no.constant=F))
acf2(resid(fit1V$fit))
fit2V <- astsa::sarima(lV, p=1,d=1,q=1)
fit3V <- astsa::sarima(lV, p=0,d=1,q=2)

##IC's?
fit1V$BIC * nn
fit2V$BIC * nn
fit3V$BIC * nn

fit1V$AIC * nn
fit2V$AIC * nn
fit3V$AIC * nn

## forecast
sarima.for(lV, n.ahead=5, p=1, d=1, q=1, no.constant=F)
## sarima.for(lV, n.ahead=5, p=1, d=0,q=1, no.constant=F)  ##compare







########
#### Air Passengers
########

?AirPassengers ##  Monthly totals of international airline passengers, 1949 to 1960; thousands
frequency(AirPassengers) ## 12
plot(AirPassengers)
(nn <- length(AirPassengers))

par(mfrow=c(3,1))
plot( (AirPassengers^(1/2)-1)/(1/2) )
plot(log(AirPassengers))
plot((AirPassengers^(-1/2)-1)/(-1/2))

lAP <- log(AirPassengers)
acf2(lAP)
acf2(diff(lAP), max.lag=80) ## max.lag=?
dlAP <- diff(lAP)
DdlAP <- diff(dlAP, lag=12)
acf2(DdlAP)
plot(DdlAP)
lag1.plot(DdlAP, 6)



## Now start fitting. #########

## astsa::sarima calls stats::arima.  stats::arima ignores the no.constant argument   (aka arima's  include.mean argument) when d or D nonzero!  So do differencing by hand if you need a mean included.

## cautious first fit: just SMA(1)
(fit0AP <- astsa::sarima(DdlAP, p=0,d=0,q=0, P=0,D=0,Q=1,S=12, no.constant=T))
acf2(fit0AP$fit$residuals)

fit1AP <- astsa::sarima(DdlAP, p=0,d=0,q=1, P=0,D=0,Q=1, S=12, no.constant=T) ## best?
acf2(fit1AP$fit$residuals)

fit2AP <- astsa::sarima(DdlAP, p=1,d=0,q=0, P=0,D=0,Q=1, S=12, no.constant=T)
fit3AP <- astsa::sarima(DdlAP, p=1,d=0,q=1, P=0,D=0,Q=1, S=12, no.constant=T)
fit4AP <- astsa::sarima(DdlAP, p=1,d=0,q=1, P=1,D=0,Q=1, S=12, no.constant=T)
## fit5AP <- astsa::sarima(DdlAP, p=0,d=0,q=2, P=0,D=0,Q=1, S=12, no.constant=F)

## compare IC's; (when d, D are the same; same transformation used)
fit0AP$AIC * nn
fit1AP$AIC * nn
fit2AP$AIC * nn
fit3AP$AIC * nn
fit4AP$AIC * nn
## fit5AP$AIC * nn

fit0AP$BIC * nn
fit1AP$BIC * nn
fit2AP$BIC * nn
fit3AP$BIC * nn
fit4AP$BIC * nn
##fit5AP$BIC * nn

## perturbations
fit1AP <- astsa::sarima(tail(lAP, -30), p=0,d=1,q=1, P=0,D=1,Q=1, S=12)
