## Charles R Doss

## ARIMA - nonstationary (d = 1)

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"


## Plot acf/pacf for a few ARIMA models
nn <- 1000
phi <- c(.4, -.5)
theta <- .5

xx <- arima.sim(model=list(order=c(2,1,0), ar=phi), n=nn)
acf2(xx)

xx <- arima.sim(model=list(order=c(0,1,1), ma=theta), n=nn)
acf2(xx)

xx <- arima.sim(model=list(order=c(2,1,1), ar=phi, ma=theta), n=nn)
acf2(xx)

## take diff
yy <- diff(xx)
acf2(yy)
