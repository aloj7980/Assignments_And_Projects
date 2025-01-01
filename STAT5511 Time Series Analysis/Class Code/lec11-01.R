
## Charles R Doss


library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"



#### exploratory data analysis, scatterplots

## the data
par(mfrow=c(2,1))
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

lag.plot(x=soi, lags=6) ## R stats
trellis.device("pdf", file=paste0(pdfpath, "autolag_scatter.pdf"))
lag1.plot(soi, 12, corr=T, smooth=T) ## astsa; not lag.plot1 (tsa.rda)
dev.off()
trellis.device("pdf", file=paste0(pdfpath, "crosslag_scatter.pdf"))
lag2.plot(soi, rec, 8, corr=T, smooth=T) ## astsa; not lag.plot2
dev.off()
lag2.plot(rec, soi, 8, corr=T, smooth=T) ## different!






