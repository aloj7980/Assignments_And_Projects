##Charles R Doss

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/" ## (my local path for saving pdfs)

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

#### chicken regression
(fit_chicken <- lm(chicken~time(chicken), na.action=NULL))
## fit_chicken <- lm(chicken~time(chicken))

## na.action preserves time series properties!
summary(fit_chicken)
trellis.device(dev="pdf", file=paste0(pdfpath, "chicken_OLS.pdf"))
plot(chicken, ylab="cents per pound")
abline(fit_chicken)
dev.off()

## chicken detrended
trellis.device(dev="pdf", file=paste0(pdfpath, "chicken_detrend.pdf"))
plot(resid(fit_chicken), ylab="cents per pound")
dev.off()


## ACFs
trellis.device(dev="pdf", file=paste0(pdfpath, "chicken_ACFs.pdf"))
par(mfrow=c(3,1))
acf(chicken, 48, ylab="ACF", main="Chicken")
acf(resid(fit_chicken), 48, ylab="ACF", main="Detrended")
acf(diff(chicken), 48, ylab="ACF", main="Differenced")
dev.off()


## comparison ACFs
acf(1:100, 48)
## do RW
TT <- 500
ww <- rnorm(n=TT)
delta <- 0
xx <- ts(delta*(1:TT) + cumsum(ww))
plot(xx, type="l")
acf(xx, 48) ## RW





####
fish <- ts.intersect(rec, soi_L6=lag(soi,-6), dframe=TRUE)
res <- lm(rec~soi_L6, data=fish, na.action=NULL)
summary(res)




#### global temps
## (note: gtemp vs. globtemp, diff series)
trellis.device(dev="pdf", file=paste0(pdfpath, "gtemp-differenced.pdf"))
par(mfrow=c(3,1))
plot(globtemp, ylab="Global Temp Dev's (C)")
plot(diff(globtemp), ylab="Diff'd Temps")
acf(diff(globtemp), main="Diff. Temp ACF")
dev.off()
mean(diff(globtemp))
acf(globtemp)## ACF gtemp

