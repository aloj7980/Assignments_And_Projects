## Charles R Doss

## Spectral density estimation for SOI and Rec

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

dev.off()

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

## can change spans; SS settle on 9
(soi.spgram.avg <- spec.pgram(soi,
                              spans=c(5), ##  vector of odd integers giving the
                                     ##  widths of modified Daniell smoothers
                              tap=0.2,
                              ## log="no",
                              pad=0, ## default
                              ## demean=F, ##default
                              detrend=T)) ##default

soi.spgram.avg$bandwidth


#######
#### kernels:
par(mfrow=c(2,2))
(myk <- kernel("daniell", m=1))
(soi.spgram.avg <- spec.pgram(soi,
                              tap=0,
                              kernel=myk,
                              log="no",
                              pad=0, ## default
                              ## demean=F, ##default
                              detrend=T)) ##default


(myk <- kernel("daniell", m=2)) ## now 'm' is SS's 'm', not SS's "L".
(myk <- kernel("daniell", m=c(21,21)))
(myk <- kernel("modified.daniell", m=5))
(myk <- kernel("modified.daniell", m=c(5,5)))
plot(myk)


(soi.spgram.avg <- spec.pgram(soi,
                              tap=0,
                              spans=NULL,
                              log="no",
                              pad=0, ## default
                              ## demean=F, ##default
                              detrend=T)) ##default
