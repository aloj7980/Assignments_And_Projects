## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R 

plot(rec)

##  haven't talked about estimation yet.
recfit <- arima(rec, c(2,0,0))

## 'predict' here is S3, using class(recfit)
preds <- predict(recfit, n.ahead=50) ## predict.Arima
## predict.ar ## alternative
ts.plot(rec, preds$pred, ylim=c(0,120))
UU <- preds$pred + 2*preds$se ## one or 2 se?
LL <- preds$pred - 2*preds$se ## one se
## cool SE plotting code
xx <- c(time(UU), rev(time(UU)))
yy <- c(LL,rev(UU))
polygon(xx,yy, border=8, col=gray(.6, .3))


stats:::predict.Arima(recfit) ## not exported from stats

## backcast
recrev = rev(rec)
recrevfit <- arima(recrev, c(2,0,0))
preds <- predict(recrevfit, n.ahead=50) ## predict.Arima
