## Charles R Doss

## "Lagged Regression", or "Transfer modeling".

## SS 4th ed, p266.

## Rec and SOI

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

soi_fit0 <- lm(soi~time(soi), na.action=NULL) ## na.action=NULL preserves ts attributes
soi_d <- (resid(soi_fit0))
acf2(soi_d) ## suggests AR(1)
(soi_d_fit1 <- arima(soi_d, order=c(1,0,0), include.mean=F)) ## detrended already
ar_soi <- as.numeric(coef(soi_d_fit1))
## get 'prewhitened' x two (same) ways
head(soi_d_pw <- resid(soi_d_fit1)) 
head(soi_d_pw_v2 <-
         filter(soi_d, filter=c(1, -ar_soi), method="convolution", sides=1)) ## same


rec_fil <- filter(rec, filter=c(1, -ar_soi), method="convolution", sides=1)
ccf(soi_d_pw, rec_fil, ylab="CCF", main="CCF(SOI, REC)", na.action=na.omit)
## suggests 5th lag
## now we forget the AR(1) fit; use the CCF for model

acf2(rec) ## perhaps suggests AR(2)  ...  but second coef turns out not to be significant

fish <- ts.intersect(rec, RL1=lag(rec,-1), RL2=lag(rec,-2), SL5=lag(soi_d, -5))
(u <- lm(fish[,1] ~ fish[,c(2,4)], na.action=NULL))
acf2(resid(u)) ## AR(1)?  MA(1)?


(fit <- astsa::sarima(fish[,1], p=1,0,0, P=0, D=0, Q=0, S=4, xreg=fish[,c(2,4)])) ##no.constant ignored if xreg used

## MA(1) worse than AR(1):
## (fit <- astsa::sarima(fish[,1], p=0,0,1,, P=0, D=0, Q=0, S=4, xreg=fish[,c(2,4)]))

(fit <- astsa::sarima(fish[,1], p=1,0,0, P=1, D=0, Q=0, S=4, xreg=fish[,c(2,4)]))

## diagnostics not great though.
## Looks like some aspect of variance/noise is modeled poorly,





acf2(fit$fit$residuals)

ccf(fit$fit$residuals, soi_d_pw)
