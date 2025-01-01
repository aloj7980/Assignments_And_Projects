## Charles R Doss
library(astsa)
library(lattice) ## plotting with trellis.device()
pdfpath <- "../../slides/figures/" ## my personal path for pdfs
########## Estimation ###############

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

#################
## model
phi <- c(1.5, -.75) ##AR coef's
theta <-  .5 ## MA coef's
mu <- 2.5
plot(ARMAacf(ar=phi, ma=theta, lag.max=30), type="h")
plot(ARMAacf(ar=phi, ma=theta, lag.max=30, pacf=T), type="h")

#################
## simulate
nn <- 50
##nn <- 1000
xx <- mu + arima.sim(model=list(ar=phi,ma=theta), n=nn)
plot(xx)

#################
##estimate
?arima
xxresML <- arima(x=xx, order=c(2,0,1),
                 method="ML",
                 include.mean=T) ##  order specified correctly!
xxresCSS <- arima(x=xx, order=c(2,0,1),
                  method="CSS",
                  include.mean=T) ##  order specified correctly!
## CSSML "==" ML
xxresCSSML <- arima(x=xx, order=c(2,0,1),
                    method="CSS-ML",
                    include.mean=T) ##  order specified correctly!
xxresCSSML
xxresML
xxresCSS
mean(xx)

plot(xxresML$resi)



## lm vs CSS (in a pure AR setting! No MA terms here!)
## no mean included
phi <- c(1.5, -.75)
theta <- 0
nn <- 50
## nn <- 5000
xx <- arima.sim(model=list(ar=phi, ma=theta), n=nn)
xxdf <- ts.intersect(xx=xx, xxL1=lag(xx,-1), xxL2=lag(xx,-2), dframe=TRUE)
(xxresLM <- lm(xx~xxL1+xxL2-1, data=xxdf)) ## duck
(xxresAROLS <- ar.ols(x=xx, aic=F, order.max=2, demean=F, intercept=FALSE)) ## duck
(xxresCSS <- arima(x=xx, order=c(2,0,0),
                  method="CSS",
                  include.mean=F)) ## duck
(xxresML <- arima(x=xx, order=c(2,0,0),
                   method="ML",
                  include.mean=F)) ## goose (sorry, I meant: gray duck)

plot(xxresML$resi)


## lm vs CSS (in a pure AR setting! No MA terms here!)
## Adding mean terms;  need to understand what is returned
(xxresLM <- lm(xx~xxL1+xxL2, data=xxdf)) ## duck
(xxresAROLS <- ar.ols(x=xx, aic=F, order.max=2, demean=F, intercept=T)) ## duck
(xxresCSS <- arima(x=xx, order=c(2,0,0),
                  method="CSS",
                  include.mean=T)) ## duck
xxresCSS$coef["intercept"] ## "intercept" = mean
xxresLM$coef[1]
.006114 * (1 - 1.389 + .684) ## intercept = mu * (1 - phi1 - phi2); adjust values



###########

phi <- c(1.5, -.75) ##AR coef's
theta <-  .5 ## MA coef's
#################
## simulate
nn <- 100
nn <- 2000
xx <- arima.sim(model=list(ar=phi,ma=theta), n=nn)
plot(xx)

## library(polynom)
## "AR(\infty)" coefficients
-ARMAtoMA(ar=-theta,
          ma=-phi,
          lag.max=10)
## run with different length xx's, different order.max's.
(xxresAR <- ar(xx, aic=F,
               method="yw",
               order.max=10))

(xxresAR <- ar(xx, aic=F,
               method="mle",
               order.max=4))




##################################
#####  can play around with intercept and mean with ar.ols
## Shouldn't intercept be 0 if mean is 0?
## read ?ar.ols

## shift mean
xx <- xx + 5

r1 <- ar.ols(xx, aic=F, order.max=2, demean=T, intercept=T)
r2 <- ar.ols(xx, aic=F, order.max=2, demean=F, intercept=T)
r3 <- ar.ols(xx, aic=F, order.max=2, demean=T, intercept=F)

rr <- r1
rr$x.mean * (1 - sum(rr$ar))
rr$x.mean
rr$x.intercept


r1$x.intercept ##non-zero, but small; of order  2 / n
r2$x.intercept


