## Charles R Doss
library(astsa)
library(lattice)
pdfpath <- "../../slides/figures/"

## for Rstudio; may need adjusting
par.old <- par(no.readonly= T)
par(mar = c(2.1, 2.1, 1.0, 1.0)) ## B L T R


head(soi)
head(rec)
par(mfrow=c(2,1))
plot(soi)
plot(rec)

dev.off()
par(mfrow=c(2,2))
soi.spgram <- spec.pgram(soi, tap=0,
                         log="no",
                         pad=0, ## default
                         ## demean=F, ##default
                         detrend=T) ##default
plot(soi.spgram, log="yes"); abline(v=1/4, lty=2)
rec.spgram <- spec.pgram(rec, tap=0,
                         log="no",
                         pad=0, ## default
                         ## demean=F, ##default
                         detrend=T) ##default
abline(v=1/4, lty=2)
plot(rec.spgram, log="yes"); abline(v=1/4, lty=2)


## SOI CI 95\% at 1/48 (48 months cycle)
qchisq(p=.025, df=2)
## [1] 0.05063562
qchisq(p=1-.025, df=2)
## [1] 7.377759
nextn(453)
(spec1 <- soi.spgram$spec[40]) ## freq 1/12 = 40/480 ($freq value (1/12)*12 = 1 per year)
(spec4 <- soi.spgram$spec[10]) ## freq 1/48 = 10/480 ($freq value (1/48)*12 = 1 per 4 years)
c( 2 * .05 / qchisq(p=1-.025, df=2),
  2 * .05 /  qchisq(p=.025, df=2) )

## at 1/12 (1 year cycle)
c( 2 * spec1 / 7.38,
  2 * spec1 / .05)

##  at (1/48)  ( 1 per 4 years  cycle)
c( 2 * spec4 / 7.38,
  2 * spec4 / .05)


