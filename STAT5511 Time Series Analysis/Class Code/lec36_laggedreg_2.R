## Charles R Doss

## "Lagged Regression", or "Transfer modeling".

## SS 4th ed, p266.
## Rec and SOI: want to regress/predict Recruitment based on SOI

## This variant is different than book: I subtract off the 1 year (12 month) periodic component from SOI which seems to help somewhat with the analysis.

## (Did not make correction to Rec yet)

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

par(mfrow=c(2,1))
plot(soi)
plot(rec)

## can do SS method with just linear detrend or can detrend SOI with sinusoidals
sin_detrend= TRUE
##sin_detrend= FALSE

## ##
## ### ##ss <- as.factor(cycle(soi))  ## consider monthly cycle in trend
## ss <- cycle(soi)
## ## ss <- (1<= ss & ss < 4) * 1 +
## ##     (4<= ss & ss < 7) * 2 +
## ##     (7<= ss & ss < 10) * 3 +
## ##     (10<= ss & ss <= 12) * 4
## ss <- as.factor(ss)
## soi_fit0 <- lm(soi~ ## time(soi) +
##                    ss, na.action=NULL) ## na.action=NULL preserves ts attributes


nn <- length(soi)
{if (sin_detrend){
     xx <- data.frame(soi=soi,
                      ## rec=rec,
                      ##rep(1, nn),
                      tt = time(soi),
                      cos_12 =cos(2 * pi * (1/12) * (1:nn)),
                      sin_12 = sin(2 * pi * (1/12) * (1:nn)))
     soi_fit0 <- lm(soi ~ tt + cos_12 + sin_12, data=xx, na.action=NULL)
     soi_d <- (resid(soi_fit0))
 } else{
     soi_fit0 <- lm(soi~time(soi), na.action=NULL) ## na.action=NULL preserves ts attributes
 }}
## acf2(soi_d)
plot(soi_fit0$fitted.values, ylim=c(-1,1))
lines(soi, col="gray")
summary(soi_fit0)
plot(resid(soi_fit0))
soi_d <- (resid(soi_fit0))
acf2(soi_d) ## suggests AR(1)?
## for demonstration's simplicity, use AR(1) for now (so don't have to compute/approximate pi)
(soi_d_fit1 <- sarima(soi_d, p=1,d=0,q=0, no.constant=T)) ## detrended already
## (soi_d_fit1 <- sarima(soi_d, p=1,d=0,q=0,
##                       P=0, D=0, Q=0, S=12,
##                       no.constant=T)) ## detrended already
## ## spec.pgram(soi_d)

soi_d_fit1$ttable
(ar_soi <- as.numeric(coef(soi_d_fit1$fit)))
## get 'prewhitened' x two (same) ways
head(soi_d_pw <- resid(soi_d_fit1$fit))
head(soi_d_pw_v2 <-
         filter(soi_d, filter=c(1, -ar_soi), method="convolution", sides=1)) ## same





rec_fil <- filter(rec, filter=c(1, -ar_soi), method="convolution", sides=1)
ccf(soi_d_pw, rec_fil, ylab="CCF", main="CCF(SOI, REC)", na.action=na.omit)
## negative lags suggest SOI leads REC
## suggests 5th lag
## now we forget the AR(1) fit; use the CCF for model

fish <- ts.intersect(rec, RL1=lag(rec,-1), RL2=lag(rec,-2), SL5=lag(soi_d, -5))
#fish <- ts.intersect(rec, RL1=lag(rec,-1), RL2=lag(rec,-2), SL5=lag(soi, -5))

## (u <- lm(rec ~ RL1+SL5, data=fish, na.action=NULL))
(u <- lm(fish[,1] ~ fish[,c(2, 4)], na.action=NULL))
##acf2(resid(u), max.lag=4*12) ## AR(1)?  MA(1)?
acf2(resid(u)) ## AR(1)?  MA(1)?

## 2nd Rec lag not significant
(uu <- astsa::sarima(fish[,1], p=1,0,0, P=0, D=0, Q=0, S=4, xreg=fish[, c(2, 3, 4)]))


(fit1000 <- astsa::sarima(fish[,1], p=1,0,0, P=0, D=0, Q=0, S=4, xreg=fish[, c(2, 4)])) ##no.constant ignored if xreg used
acf2(fit1000$fit$residuals)
(fit0100 <- astsa::sarima(fish[,1], p=0,0,1,, P=0, D=0, Q=0, S=4, xreg=fish[,c(2,4)]))## MA(1) worse than AR(1)
(fit1010 <- astsa::sarima(fish[,1], p=1,0,0, P=1, D=0, Q=0, S=4, xreg=fish[,c(2,4)]))
(fit1001 <- astsa::sarima(fish[,1], p=1,0,0, P=0, D=0, Q=1, S=4, xreg=fish[,c(2,4)]))

acf2(fit1001$fit$resid, max.lag=144)

BIC(fit1000$fit,
    fit0100$fit,
    fit1010$fit,
    fit1001$fit)


AIC(fit1000$fit,
    fit1010$fit,
    fit1010$fit,
    fit1001$fit)

## Can use arima() to exclude intercept if desired.  Potentially less common
## to do so when including other linear regressors, just as in regular linear
## regression.


## nn <- length(fish[,1])
## fit1000$BIC * nn
## fit1010$BIC * nn


## diagnostics not great though.
## Looks like some aspect of variance/noise is modeled poorly,



## ## a few last checks to consider
## fit <- ;
## acf2(fit$fit$residuals)
## ccf(fit$fit$residuals, soi_d_pw)



