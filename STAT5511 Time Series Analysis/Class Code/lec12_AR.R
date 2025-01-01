## Charles R Doss

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"





################################
################ Ch3 ###########
################################

#### AR sims
dev.off()
nn <- 500

phitrue <- .9
AR <- arima.sim(model=list(order=c(1,0,0), ar=phitrue),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
par(mfrow=c(2,1))
plot(ts(rnorm(nn)), ylab="(Gaussian) White Noise")
plot(AR, ylab="AR(1)")

dev.off()
phitrue <- .3 ## harder to distinguish WN and AR(1)
AR <- arima.sim(model=list(order=c(1,0,0), ar=phitrue),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
par(mfrow=c(2,1))
plot(ts(rnorm(nn)), ylab="(Gaussian) White Noise")
plot(AR, ylab="AR(1)")

dev.off()
phitrue <- -.9 ## harder to distinguish WN and AR(1)
AR <- arima.sim(model=list(order=c(1,0,0), ar=phitrue),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
par(mfrow=c(2,1))
plot(ts(rnorm(nn)),
     ## xlim=c(0,50),
     ylab="(Gaussian) White Noise")
plot(AR,
     ##xlim=c(0,50),
     ylab="AR(1)")



###### Plot AR ACFs

dev.off()
acftrue <- function(hh){phitrue^hh} ## bad programming practice
hh <- 0:20

phitrue <- .9
plot(hh,acftrue(hh), type="h")

phitrue <- .5
plot(hh,acftrue(hh), type="h")

phitrue <- .1
plot(hh,acftrue(hh), type="h")


phitrue <- -.9
plot(hh,acftrue(hh), type="h")

