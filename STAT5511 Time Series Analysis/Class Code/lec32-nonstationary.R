## Charles R Doss

## "Other unit roots" (besides 1)

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
#library(sarima) ## simulate: sim_arima()
#library(forecast)

nn <- 500
ww <- rnorm(nn)
xx <- filter(x=ww, filter=c(-1), method="recursive", sides=1) 
acf2(xx) ## non-stationary, but non-unit root

yy <- diff(xx, 2) ## seasonal difference 
acf2(yy) ## stationary (... although, non-invertible)


## the obvious alternative option in this particular case;
## not generalizable though.
zz <- filter(xx, filter=c(1,1), method="convolution", sides=1)
acf2(zz)

