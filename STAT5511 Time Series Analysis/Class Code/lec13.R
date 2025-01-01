
## Charles R Doss


library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

par.old <- par(no.readonly= T)
par(mar = c(1.5, 2, 1.5, 1.5)) ## Rstudio; not quite perfect

#### MA sims
nn <- 500

set.seed(1)
MA.5 <- arima.sim(model=list(ma=.5),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
plot(MA.5)

set.seed(1)
MA.5.2 <- arima.sim(model=list(order=c(0,0,1), ma=.5),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
par(mfrow=c(2,1))
plot(MA.5)
plot(MA.5.2)
head(MA.5-MA.5.2) ##identical


## now do plots
mytheta <- .4
MA <- arima.sim(model=list(ma=.4),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
trellis.device("pdf", file=paste0(pdfpath, "MA1paths1.pdf"))
par(mfrow=c(2,1))
plot(ts(rnorm(nn)), ylab="(Gaussian) White Noise")
plot(MA,
     main=expression(paste(theta1, "=.4")),
     ylab="MA(1)")
dev.off()

mytheta <- -.9
MA <- arima.sim(model=list(ma=mytheta),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)
trellis.device("pdf", file=paste0(pdfpath, "MA1paths2.pdf"))
par(mfrow=c(2,1))
plot(ts(rnorm(nn)), ylab="(Gaussian) White Noise")
plot(MA,
     main=expression(paste(theta1, "=.9")),
     ylab="MA(1)")
dev.off()


mytheta <- .9
MA <- arima.sim(model=list(ma=mytheta),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)


AR <- arima.sim(model=list(ar=.5),
                  n=nn,
                  rand.gen=rnorm, mean=0, sd=1)

trellis.device("pdf", file=paste0(pdfpath, "MA1paths2.pdf"))
par(mfrow=c(2,1))
plot(AR, ylab="AR(1)")
plot(MA,
     main=expression(paste(theta1, "=.9")),
     ylab="MA(1)")
dev.off()







#### MA ACFs
dev.off()
acftrue <- function(hh){
    res <- rep(0, length=length(hh))
    res[hh==0] <- 1
    res[abs(hh)==1] <- mytheta/(1+mytheta^2) ## bad programming practice
    res
}
hh <- 0:6

mytheta <- 1/5
plot(hh, acftrue(hh), type="h")


mytheta <- .9
plot(hh, acftrue(hh), type="h")


mytheta <- -.9
plot(hh, acftrue(hh), type="h")
