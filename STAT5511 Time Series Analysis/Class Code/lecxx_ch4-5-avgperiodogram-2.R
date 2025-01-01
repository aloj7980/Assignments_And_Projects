## Charles R Doss


library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

dev.off()

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R


################
## true distribution
phi <- .85
theta <- c(-.8, .3, .15)
dd <- 0


############
## simulate the 'data'

set.seed(108)
nn <- 951
xx <- arima.sim(model=list(order=c(length(phi), dd, length(theta)), ar=phi, ma=theta),
                n=nn,
                rand.gen=rnorm)
plot(xx)

## library(TSA) ## ARMAspec

trueARMAspec <- TSA::ARMAspec(model = list(ar=phi, ma=theta),
                              lty=2) ## function for plotting true
                                              ## arma spectral density (can
                                              ## see formula from SS)

spg <- spec.pgram(x=xx,  taper=0, log="no", detrend=FALSE,
                  add=TRUE, lwd=.5, col="gray")


spec.pgram(x=xx, spans=15, taper=0, log="no", detrend=FALSE,
           add=TRUE, col="red")

spec.pgram(x=xx, spans=33, taper=0, log="no", detrend=FALSE,
           add=TRUE, col="red")

spec.pgram(x=xx, spans=71, taper=0, log="no", detrend=FALSE,
           add=TRUE, col="blue")


spec.pgram(x=xx, spans=171, taper=0, log="no", detrend=FALSE,
           add=TRUE, col="orange")


## from lec39_ch4-4-avgperiodogram.R;
##
(myk <- kernel("daniell", m=5)) ## now 'm' is SS's 'm', not SS's "L".
(myk <- kernel("daniell", m=c(2,2)))
(myk <- kernel("modified.daniell", m=5))
(myk <- kernel("modified.daniell", m=c(10,10)))
(myk <- kernel("modified.daniell", m=c(10,10,10)))
## plot(myk)



(myk <- kernel("modified.daniell", m=c(4,4)))
## plot(myk)
spec.pgram(x=xx, kernel=myk,
           taper=0, log="no", detrend=FALSE,
           add=TRUE, col="purple")


(myk <- kernel("modified.daniell", m=c(30,30)))
## plot(myk)
spec.pgram(x=xx, kernel=myk,
           taper=0, log="no", detrend=FALSE,
           add=TRUE, col="green") ## accurate to the right; but not on lower freqs


(myk <- kernel("modified.daniell", m=c(10, 10)))
## plot(myk)
spec.pgram(x=xx, kernel=myk,
           taper=0, log="no", detrend=FALSE,
           add=TRUE, col="darkgreen") ## wavy on the right; pretty accurate on left




#######
####### CIs
#######


spec.pgram(x=xx, ## kernel=myk,
           taper=0, log="yes", detrend=FALSE,
           lwd=.5, col="gray")



myk <- kernel("modified.daniell", m=c(10, 10))
spec.pgram(x=xx, kernel=myk,
           taper=0, log="yes", detrend=FALSE,
           lwd=.5, col="gray")


LL <- 21 ## m=10
df <- 2 * LL ## if no padding
log(qchisq(p=.975, df=df)) - log(qchisq(p=.025, df=df))


## Note: if L or m is very large the CI is *always* very small ... !!!  It does
## not account for bias.  So must be interpreted carefully.
myk <- kernel("modified.daniell",
              m=c(100, 100)) ## can run this code with different values for m
spec.pgram(x=xx, kernel=myk,
           ## ylim=c(-10,3),
           ylim=c(.2,11), ## so can see length of CI change
           taper=0, log="yes", detrend=FALSE,
           lwd=.5, col="gray")


## the CI here is based on
df.kernel(myk)

df.kernel(kernel("daniell", 5)) ## 2*L = 2*(2 * m + 1)
