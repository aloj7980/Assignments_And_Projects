## Could be used as 5511 example local level smoother , how conditional
## variance of latent var changes for missing data

## Playing with a few questions about P (DLM X_t error)

library(dlm)
library(astsa)
library(data.table)
library(ggplot2)
source("class/lecxx_dlm_locallevel_functions.R")




######################################################
### One dim local level
######################################################

mym0 <- gtemp_land[1]
dlmres <- dlmMLE(y = gtemp_land,
                 parm = c(0,0),
                 m0 = mym0,
                 control = list(maxit=300),
                 build = locallevel_1_1)
dlmres$par
mod <- locallevel_1_1(x=dlmres$par, m0=mym0)
res_filter <- dlmFilter( y = gtemp_land, mod = mod)
res_smoother <- dlmSmooth( y = gtemp_land +
                         rnorm(n=174, mean=0, sd = .1),
                          mod = mymod)
res_smoother <- dlmSmooth(res_filter)

(gtemp_f <- res_filter$m[-1])
gtemp_f <- ts(data= gtemp_f, start=1850, end=2023, frequency=1)
gtemp_s <- res_smoother$s[-1]
gtemp_s <- ts(data= gtemp_s, start=1850, end=2023, frequency=1)

## Plots
plot(gtemp_land,
     type="o")
lines(gtemp_f, type="o", col="blue")
lines(gtemp_s, type="o", col="red")



######################################################
### Two dim local level
######################################################

## gtemp_land, gtemp_ocean
gtemp <- rbind(gtemp_land, gtemp_ocean)
mym02 <- matrix(mean(gtemp[,1]))

dlmres_2 <- dlmMLE(y = (gtemp),
                   parm = c(0,0,0),
                   m0 = mym02,
                   control = list(maxit=300),
                   build = locallevel_2_1)
dlmres_2$par
(mod2 <- locallevel_2_1(x = dlmres_2$par, m0=(mym02)))
res_filter2 <- dlmFilter(y = t(gtemp), mod = mod2)
dlmSmooth(t(gtemp), mod=mod2, debug=T)
res_smoother2 <- dlmSmooth(res_filter2)

## output filter values ; first one has to be dropped
gtemp_f_2 <- ts(res_filter2$m[-1], start=1850, end=2023, freq=1)
gtemp_s_2 <- ts(res_smoother2$s[-1], start=1850, end=2023, freq=1)

## Plots
plot(gtemp_land,
     type="o")
lines(gtemp_ocean, type="o", lty=2)
##lines(gtemp_f, type="o", col="blue")
lines(gtemp_s, type="o", col="gray", )
lines(gtemp_f_2, type="o", col="blue")
lines(gtemp_s_2, type="o", col="red")



######################################################
###some extra sim below
######################################################


set.seed(50)
nn <- 475
mu <- 3.2
phi <- .5
## theta <- c(.4, .6, .25)
## theta <- -c(.4, .6, .25)
theta <- -c(.5, 0, .25)
ARMAtoMA(ar=phi, ma=theta, lag.max=10)

plot(xx <- mu + arima.sim(model =
                            list(ar = phi, ma = theta, order = c(length(phi), 1, length(theta))),
                          n=nn))
xx2 <- mu + arima.sim(model =
                            list(ar = phi, ma = theta, order = c(length(phi), 1, length(theta))),
                      n=nn)
XX2 <- rbind(xx,xx2)


dlmres <- dlmMLE(y = XX2,
                 parm = c(0,0,0),
                 m0 = xx[1],
                 control = list(maxit=300),
                 build = locallevel_2_1)



dlmres <- dlmMLE(y = xx,
                 parm = c(0,0),
                 m0 = xx[1],
                 control = list(maxit=300),
                 build = locallevel_1_1)

dlmres$par
mod <- locallevel_1_1(x=dlmres$par, m0=mym0)
res_filter <- dlmFilter( y = gtemp_land, mod = mod)

res_smoother <- dlmSmooth( y = gtemp_land, mod = mod)

res_smoother <- dlmSmooth(res_filter)
