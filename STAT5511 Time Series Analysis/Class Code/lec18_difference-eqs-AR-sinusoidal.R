## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"



## see pp 92 SS
z <- c(1, -1.5, .75) ## coefs
(a <- polyroot(z)[1])
arg <- Arg(a)/(2*pi)
1/arg ## period

ar2 <- arima.sim(list(order=c(2,0,0), ar=c(1.5, -.75)), ##negative of z
                 n=144)
plot(ar2, axes=FALSE, xlab="Time")
axis(2)
axis(1, at=seq(0, 144, by=12))
box()
abline(v=seq(0,144,by=12), lty=2)

##ACF:
ACF <- ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)



############################
############################
yy2 <- c(1, c(-.56, .82)) ## ARpolynomial coefs; see 'p2'
(a <- polyroot(yy2)[1])
arg <- Arg(a)/(2*pi)
1/arg ## period
nn <- 144
ar2 <- arima.sim(list(order=c(2,0,0), ar=-yy2[-1]),
                 n=nn)
plot(ar2, axes=FALSE, xlab="Time")
axis(2)
#axis(1, at=seq(0, nn, by=12))
axis(1, at=seq(0, nn, by=5))
box()
##abline(v=seq(0,nn,by=12), lty=2)
abline(v=seq(0,nn,by=5), lty=2)

ACF <- ARMAacf(ar=-yy2[-1], ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)



############################
############################
yy3 <- c(1, c(-2.13, 2.53, -1.77, .683)) ## ARpolynomial coefs
(a <- polyroot(yy3)[1])
arg <- Arg(a)/(2*pi)
1/arg ## period

arg <- Arg(polyroot(yy3)[2]) / (2*pi)
1/arg ## period

nn <- 3*144
ar2 <- arima.sim(list(order=c(4,0,0), ar=-yy3[-1]),
                 n=nn)

plot(ar2, axes=FALSE, xlab="Time")
axis(2)
#axis(1, at=seq(0, nn, by=12))
axis(1, at=seq(0, nn, by=5))
box()
##abline(v=seq(0,nn,by=12), lty=2)
abline(v=seq(0,nn,by=5), lty=2)

##ACF:
ACF <- ARMAacf(ar=-yy3[-1], ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)



## plot acfs together
par(mfrow=c(3,1))

ACF <- ARMAacf(ar=c(1.5,-.75), ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)


ACF <- ARMAacf(ar=-yy2[-1], ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)

ACF <- ARMAacf(ar=-yy3[-1], ma=0, 50)
plot(ACF, type="h", xlab="lag")
abline(h=0)
