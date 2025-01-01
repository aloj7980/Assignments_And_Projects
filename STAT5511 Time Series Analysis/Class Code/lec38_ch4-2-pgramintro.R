## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R


#sinusoids from prev file

nn <- 102
## nn <- 100
tt <- 1:nn
x1 = 2*cos(2*pi* tt *6/100) + 3*sin(2*pi* tt *6/100)
x2 = 4*cos(2*pi* tt *10/100) + 5*sin(2*pi* tt *10/100)
x3 = 6*cos(2*pi* tt *40/100) + 7*sin(2*pi* tt *40/100)
x0 = x1+x2+x3
x <- x0 + rnorm(nn, sd=8) ## doesn't exactly correspond to anything (not arma data, not gamma)
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
plot.ts(x,  ylim=c(-16,16), main="sum")
lines(x0,  lty=2, col="red")


## X <- fft(x)
## fq <- 2*pi/nn
## frq <- 0
## FL <- 0
## FL[1] <- X[1]^2 / nn^2
## frq[1] <- 0
## for (jj in 2:(nn/2)){
##     FL[jj] <- 2 * (X[jj] * X[nn+2 - jj]) / nn^2
##     frq[jj] <- fq * (jj-1)
## }
## FL[nn/2 + 1] <-  X[nn/2 + 1]^2 / nn^2
## frq[nn/2 + 1] <- pi
## plot(frq,FL)


############# fft() analysis

#### Start w/ analysis of x0, truth.
par(mfrow=c(2,2)) ## executed above already
x0.fft <- fft(x0)
?fft ## scaling: factor nn, and exp(-2 pi i / n) [h is same, just indexed
     ## differently]
x0.pgram <- Mod(x0.fft)^2 / nn
x0.spgram <- x0.pgram * 4 / nn
frqs <- (0:(nn-1))/nn ## Fundamental freqs
plot(frqs, x0.pgram, type="h", ylab="periodogram")
plot(frqs, x0.spgram, type="h", ylab="scaled periodogam")
## change domain; change plot type
## indcs <- 2:51 ## 1st index is just mean; 0 if demeaned; not plotted.
indcs <- 2:(nn/2 +1) ## 1st index is just mean; 0 if demeaned; not plotted.
plot(frqs[indcs], x0.pgram[indcs],
     type="h",
     ylab="periodogram")
plot(frqs[indcs], x0.spgram[indcs], type="h", ylab="scaled periodogam")



## add x
x.pgram <- Mod(fft(x))^2 / nn
x.spgram <- x.pgram * 4 / nn
dev.off()
par(mfrow=c(2,1))
plot(frqs[indcs], x0.spgram[indcs],
     type="h",
     ylab="noiseless scaled periodogram")
plot(frqs[indcs], x.spgram[indcs],
     type="h",
     ylab="noisy scaled periodogram")


## sum related to variance
sum(x.spgram) / 4
var(x) * (nn-1)/nn




################# spec.pgram() ###

par(mfrow=c(2,1))
plot(frqs[indcs], x.spgram[indcs],
     type="h",
     ylab="noisy scaled periodogram")


## spectrum() calls spec.pgram()
x.pgram.res1 <- spec.pgram(x=x)
x.pgram.res2 <- spec.pgram(x=x, taper=0)
x.pgram.res3 <- spec.pgram(x=x,  taper=0, log="no")
x.pgram.res2$spec - x.pgram.res3$spec ## log is just a plotting parameter!
## function used for plotting, in case there are questions... :
##stats:::plot.spec ## not visible!
##stats:::plot.spec(x.pgram.res2)
## plot(x.pgram.res2) ## same output
plot(x.pgram.res3$freq, log10(x.pgram.res2$spec), type="l") ## comparison; y-label!
## how best to visually compare  to our by-hand plot?
x.pgram.res5 <- spec.pgram(x=x, log="no", taper=0, detrend=F, type="h") ## type = h (detrend=F)

#### specify m=0 to get (unsmoothed) periodogram
## x.pgram.res6 <- spec.pgram(x=x,
##                            kernel=kernel("daniell", m=0),
##                            log="no",
##                            taper=0, detrend=F)


## now re-define x with sample size 102
dev.off()
par(mfrow=c(2,2)) ## executed above already
x2.pgram.res1 <- spec.pgram(x=x0, log="no", taper=0, detrend=F, type="h")
x2.pgram.res1$n.used
(nnp <- nextn(nn))
## So when getting output, need to be

x.pad <- c(x0, rep(0, nnp-nn))
x.fft <- fft(x.pad)

#### recreate output from spec.pgram via fft() and padding-by-hand.
x.pgram <- Mod(x.fft)^2 / nn ## NOT nnp!
## x.spgram <- x.pgram * 4 / nnp
frqs <- (0:(nnp-1))/nnp
indcs <- 2:(nnp/2 + 1)
plot(frqs[indcs], x.pgram[indcs],
     type="h",
     ylab="periodogram")
sum(abs(x.pgram[indcs] - x2.pgram.res1$spec))
