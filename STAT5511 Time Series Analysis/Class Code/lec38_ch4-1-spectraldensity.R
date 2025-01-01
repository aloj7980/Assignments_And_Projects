## Charles R Doss

## plotting (sums of) sinusoids, and
## plotting spectral densities of AR and MA processes.

library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"


## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R

########################
####sinusoids

## generate (without any random noise) sinusoids; think of these perhaps as
## ACFs rather than data series.
nn <- 100
tt <- 1:nn
x1 = 2*cos(2*pi* tt *6/100) + 3*sin(2*pi* tt *6/100)
x2 = 4*cos(2*pi* tt *10/100) + 5*sin(2*pi* tt *10/100)
x3 = 6*cos(2*pi* tt *40/100) + 7*sin(2*pi* tt *40/100)
x =x1+x2+x3 ## not periodic [by time 100]; but has periodicities / frequency components
pdf("sinusoids.pdf")
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
plot.ts(x,  ylim=c(-16,16), main="sum")
dev.off()

########################
#### spectral density

pdf("AR1-spectral-1.pdf")
ph=.9
v=seq(0,1/2,.001)
f=1/(1-2*ph*cos(2*pi*v)+ph^2)
plot(v,f,type="l")
dev.off()

pdf("AR1-spectral-2.pdf")
ph = -.9
v=seq(0,1/2,.001)
f=1/(1-2*ph*cos(2*pi*v)+ph^2)
plot(v,f,type="l")
dev.off()


pdf("MA1-spectral-1.pdf")
th=.9
f=(1 + th^2 + 2*th*cos(2*pi*v))
plot(v,f,type="l")
dev.off()

pdf("MA1-spectral-2.pdf")
th=-.9
f=(1+th^2+2*th*cos(2*pi*v))
plot(v,f, type="l")
dev.off()
