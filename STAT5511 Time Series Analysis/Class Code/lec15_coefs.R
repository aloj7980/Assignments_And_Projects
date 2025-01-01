##Charles R Doss ####################################
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"
######################################################

## Causal representation:
ARMAtoMA(ar=.9, ma=.5, lag.max=10) ## .9 not -.9



## Inverted representation:
ARMAtoMA(ar=-.5, ma=-.9, lag.max=10)
