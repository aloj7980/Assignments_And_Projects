##Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

## Same block of code, repeated.

## AR(1), .1
par(mfrow=c(2,1))
phi <- .5 ## \pm .1; .5; .9;
theta <- 0
plot(ARMAacf(ar=phi, ma=theta, pacf=F, 24)[-1],
     type="h", ylab="ACF")
plot(ARMAacf(ar=phi, ma=theta, pacf=T,24),
     type="h", ylab="PACF")


## AR(2)
par(mfrow=c(2,1))
phi <- c(1.5, -.75)
theta <- 0
## phi <- -c(1.5, -.75) ## what about this
plot(ARMAacf(ar=phi, ma=theta, pacf=F, 24)[-1],
     type="h", ylab="ACF")
plot(ARMAacf(ar=phi, ma=theta, pacf=T,24),
     type="h", ylab="PACF"    )




## MA(1), MA(2), MA(3).
par(mfrow=c(2,1))
phi <- 0
theta <- -.5
## theta <- c(-.5, .6)
## theta <- c(-.5, .5, .9) ## theta[2] is *not* 0
plot(ARMAacf(ar=phi, ma=theta, pacf=F, 24)[-1],
     type="h", ylab="ACF")
plot(ARMAacf(ar=phi, ma=theta, pacf=T,24),
     type="h", ylab="PACF")


## ARMA (various p,q).
par(mfrow=c(2,1))
phi <- c(1.5, -.75)
theta <- -.5
##theta <- c(-.5, .5, .9)
plot(ARMAacf(ar=phi, ma=theta, pacf=F, 24)[-1],
     type="h", ylab="ACF")
plot(ARMAacf(ar=phi, ma=theta, pacf=T,24),
     type="h", ylab="PACF")

## ARMA(1,1)?



## Recruitment

par(mfrow=c(2,1))
acf(rec,48)
acf(rec,48, type="partial") ## just the stats package
acf2(rec, 96) ## astsa package

acf2(soi)
