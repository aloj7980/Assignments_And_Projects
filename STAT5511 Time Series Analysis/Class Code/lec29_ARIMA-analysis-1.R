## Charles R. Doss
## STAT 5511
## Lec 29

library(astsa)
set.seed(99) ## I guess seed changed at some point but results are similar



################
## true distribution
phi <- NULL
theta <- c(.4, .3, .15)
dd <- 0
ARMAacf(ar=phi, ma=theta, lag.max=10)
ARMAacf(ar=phi, ma=theta, lag.max=10, pacf=T)




############
## simulate the 'data'
nn <- 600
xx <- arima.sim(model=list(order=c(length(phi), dd, length(theta)), ar=phi, ma=theta),
                n=nn,
                rand.gen=rnorm)
plot(xx)

invisible(acf2(xx))

## dxx <- diff(xx)


## For simplicity/time's sake, I won't compare with and without the constant/intercept
MM <- 4
fits_all <- vector("list", length=MM)
AICmin <- BICmin<- Inf
for (ii in 1:MM){ ## ii-1, jj-1 are orders
    for (jj in 1:MM){
        fits_all[[ii]][[jj]] <- sarima(xx, p=ii-1, d=0, q=jj-1, details=F)
        if (fits_all[[ii]][[jj]]$AICc < AICmin) AICmin <- fits_all[[ii]][[jj]]$AICc
        if (fits_all[[ii]][[jj]]$BIC < BICmin) BICmin <- fits_all[[ii]][[jj]]$BIC
    }
}

################
## look at IC output
for (ii in 1:MM){
    for (jj in 1:MM){
        print(paste0(ii-1, " ", jj-1))
        print( ((fits_all[[ii]][[jj]])$AICc - AICmin) * nn )
        ##  print( ((fits_all[[ii]][[jj]])$BIC - BICmin ) * nn)
    }
}
## 2 / nn ## rough IC 'equivalence' (if don't scale up sarima()'s IC's by nn)

## AICc Hits: (1,0), (0,3), (1,2), (1,1) , ...  (3,1), (3,2)
## BIC Hits: (1,0), (1,1)?


################
## Examine residual plots
res <- invisible(sarima(xx, p=0, d=0, q=3))

## From resids, probably (0,3) looks best, maybe (1,0) in consideration

################
## Consider p-values
fits_all[[0 + 1]][[3 + 1]]$ttable ##0,3
fits_all[[1 + 1]][[0 + 1]]$ttable ##1,0
fits_all[[3 + 1]][[1 + 1]]$ttable ##3,1


fits_all[[1 + 1]][[0 + 1]]$fit

################
## Examine lagged plots
res <- sarima(xx, p=1, d=0, q=0)
lag1.plot(res$fit$residuals, max.lag=12)

################
## Find 95% CI's for a fit
myres <- fits_all[[0 + 1]][[3 + 1]] ##0,3
coefs <- myres$fit$coef
SEs <- sqrt(diag(myres$fit$var.coef))
rbind(coefs, ## estimate
      coefs -  1.96 * SEs, ## lower CI bound
      coefs +  1.96 * SEs) ## upper CI bound


################
## some extra analysis
ARMAtoMA(ar = .5191, ma=0, lag.max=10)




## Summary: (0,3) and (1,0) are very similar fits actually; former is better,
## so AIC prefers; but latter is much more parsimonious, so BIC prefers.


############################################################
############################################################


## Now re-do analysis but with
dd <- 1



############################################################
############################################################

## To build intuition and experience, can do similar simulations but with:
                                        # different ARIMA specifications
                                        # different sample sizes nn
                                        # different error distributions (rand.gen)
                                        # (and even non-iid white noise series (innov))
