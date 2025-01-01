## Charles R Doss
library(astsa)
library(lattice) ## plotting with trellis.device()
pdfpath <- "../../slides/figures/" ## my personal path for pdfs
########## Estimation ###############

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

phi <- c(1.5, -.75) ##AR coef's
theta <-  .5 ## MA coef's
mu <- 2.5

#################
## simulate
nn <- 50
##nn <- 1000
xx <- mu + arima.sim(model=list(ar=phi,ma=theta), n=nn)
plot(xx)

xxresML <- arima(x=xx, order=c(2,0,1),
                 method="ML",
                 include.mean=T) ##  order specified correctly!


## All code above here is identical to previous lecture code

##############
## variances
xxresML$var.coef ## anything strange?
(1 - xxresML$coef[2]^2) / nn ## Compare !
##  From ?arima:
## "The variance matrix of the estimates is found from the Hessian of the
##      log-likelihood, and so may only be a rough guide."
