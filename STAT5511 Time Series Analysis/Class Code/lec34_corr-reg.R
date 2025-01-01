## Charles R Doss

## Regression with correlated errors; fixed covariates.

## SS 4th ed, page 144, Ex 3.44 (see also Ex 2.2)

## relating (mean adjusted) temperature and particulate levels  to mortality

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"


##data:
## Mortality in LA
cmort; ## weekly cardio mortality
tempr; ## temperature, Fahrenheit
part; ##

par(mfrow=c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temp", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")

## this model was argued for earlier in the textbook
trend <- time(cmort)
temp <- tempr-mean(tempr)
temp2 <- temp^2
summary(fit <- lm(cmort~trend+temp+temp2+part, na.action=NULL))
acf2(resid(fit), 52) ## suggests AR(2)

## do the regression fit, and examine diagnostics:
astsa::sarima(cmort, p=2,d=0,q=0,
              ## no.constant=TRUE, ## this would be ignored;
              xreg=cbind(trend,temp,temp2,part))
## if you want to exclude intercept for some reason, see ?stats::arima(); can
## use no.intercept argument there in combination with xreg.  For some reason
## astsa::sarima does not implement this.
