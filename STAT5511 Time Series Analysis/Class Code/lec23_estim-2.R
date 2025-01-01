## Charles R Doss
library(astsa)
library(lattice) ## plotting with trellis.device()
pdfpath <- "../../slides/figures/" ## my personal path for pdfs

########## Estimation ###############

###### investigating the idea of iteratively fitting models ...


## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

####### model 1
phi <- c(1.5, -.75) ##AR coef's
## simulate
nn <- 500
xx <- arima.sim(model=list(ar=phi), n=nn)
plot(xx)
##estimate
xxresML <- arima(x=xx, order=c(1,0,0),
                 method="ML",
                 include.mean=T) ##  
acf2(xxresML$resi) ## acf of residuals; messy.
polyroot(c(1, -1.5, .75)) ## complex roots: so can't ever get real factorization, ... makes idea



####### model 2
a1  <- .8
a2 <- .6
## multiply polynomials (1 - a1 z) (1 - a2 z); then do algebra
(phi <- c(a1+a2, -a1*a2)) ##AR coef's
## simulate
nn <- 500
xx <- arima.sim(model=list(ar=phi), n=nn)
plot(xx)
##estimate
xxresML <- arima(x=xx, order=c(1,0,0),
                 method="ML",
                 include.mean=T) ##  breaks (b/c looks like phi1 =1)
acf2(xxresML$resi) ## 
xxresML$coef ## 
zz <- filter(xx, filter=c(1, -a1),  "convolution", sides=1)
acf2(zz) ## if we estimated one of the *ROOTS* perfectly
zz <- filter(xx, filter=c(1, -phi[1]),  "convolution", sides=1)
acf2(zz) ## plug in the truth (which is non-stationary?!) [confusing!]

## plug in arbitrary value
zz <- filter(xx, filter=c(1, -.2),  "convolution", sides=1)
acf2(zz)



