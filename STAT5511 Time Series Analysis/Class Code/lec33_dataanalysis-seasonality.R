## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
## library(sarima) ## simulate: sim_arima() ## trustworthy ?
library(forecast)






###### NOTE TO SELF : sim_sarima seems to give strange results; acf and pacf
###### are almost equal ??  Also note that there is no signal in the acf after twice differencing which makes no sense ?




################################################################
## seasonal component (and potentially nonstationarity)
################################################################


## Consider: Can review (nonseasonal) ARMA ACF/PACF.  Then examine
## seasonal ARMA model ACF/PACF with AR(1), MA(1), ARMA(1,1), and ARIMA(0,1,0); as
## well as multiplicative versions.
ticks <- c(1, (1:20)*12)
PHI1 <- .4
## PHI1 <- 0 ## compare
## phi <- c(rep(0,11), PHI1)
phi1 <- -.6
phi <- c(phi1, rep(0,10), PHI1, -phi1*PHI1)
## phi <- -.6
## theta <- -.5
theta <- 0
ACF <- ARMAacf(ar=phi, ma=theta, 100)[-1]
PACF <- ARMAacf(ar=phi, ma=theta, 100, pacf=TRUE)
par(mfrow=c(2,1))
plot(ACF, type="h", xlab="lag", xaxt="n",
     ylim=c(-.4, 1.05))
axis(1, at=ticks)
abline(h=0)
plot(PACF, type="h", xlab="lag", xaxt="n",
     ylim=c(-.4, 1.05))
axis(1, at=ticks)
abline(h=0)

plot(xx <- arima.sim(model=list(ar=phi, ma=theta), n=288))

## (0,0,1) x (1,0,0)_{12}



################################################################
## seasonal nonstationarity
################################################################




#### doing simulation by hand

#### can also compare with astsa::sarima.sim() in newer version of astsa
#### package.

set.seed(10021)
ss <- 12
## simulate arma with seasonality
phi <- c(phi1, rep(0,ss-2), PHI1, -phi1*PHI1)
## phi <- -.6
theta <- -.5
##theta <- 0
nn <- 500
plot(xx <- arima.sim(model=list(ar=phi, ma=theta), n=nn))

## integrate
tmp <- diffinv(xx, lag=ss)[-(1:(2*ss))] ## 10 zeros prepended and then drop the
                                   ## first 10 entries as well which are just
                                   ## xx
yy <- as.ts(diffinv(tmp, lag=1)[-(1:2)])
plot(yy)



##### alternatively, can use sarima.sim function from astsa for simulation
##### (new function, apparently they have not written help for it).
{
    ## no ? help available
    nn = 500
    yy <- sarima.sim(ar = .5, d=1, ma=.3, sar=-.5, D=1, sma=-.4, S=12,
                     n=nn)
    plot(yy)
}


acf2(yy, max.lag=70)
## acf2(diff(yy), max.lag=70)
dyy <- diff(yy)
acf2(dyy, max.lag=70)
Ddyy <- diff(dyy, lag=ss)
acf2(diff(yy, lag=ss), max.lag=70) ## if you do seasonal diff first
acf2(Ddyy, max.lag=70)

## sometimes the seasonal difference stands out more, initially, on the acf,
## sometimes other way around.

## Can fit as usual using sarima() and get usual diagnostics
## here we can for instance examine the IC's
(res1 <- astsa::sarima(yy, p=0,d=1,q=0, P=1,D=1,Q=0, S=ss))
(res2 <- astsa::sarima(yy, p=0,d=1,q=0, P=1,D=1,Q=1, S=ss))

(res3 <- astsa::sarima(yy, p=1,d=1,q=0, P=1,D=1,Q=0, S=ss))




 ## Note: d+D>1: no intercept by default;
(res1 <- astsa::sarima(yy, p=0,d=1,q=0, P=1,D=1,Q=0, S=12))
## in other packages (e.g., sarima::sarima) may also need to check








########################################################################
#### Air Passengers
########################################################################

?AirPassengers ##  Monthly totals of international airline passengers, 1949 to 1960; thousands
frequency(AirPassengers) ## 12
plot(AirPassengers)
(nn <- length(AirPassengers))

par(mfrow=c(3,1))
plot( (AirPassengers^(1/2)-1)/(1/2) )
plot(log(AirPassengers))
plot((AirPassengers^(-1/2)-1)/(-1/2))

lAP <- log(AirPassengers) ## variance stabilizing
acf2(lAP)
dlAP <- diff(lAP)
plot(dlAP)
acf2(dlAP, max.lag=60) ## max.lag=?
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



