####################################################################################################
## Bootstrap for regression
####################################################################################################

load("~/Documents/Classes/Fall2023/5610/data/AudiA4.rda")

AudiA4 <- AudiA4[,c("year","price","mileage","distance")]

str(AudiA4)

## Fit to everything
# straight up linear
fit <- lm(price~year+mileage+distance,data=AudiA4)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

# add some nonlinearity
fit <- lm(price~poly(year,degree=2)+poly(mileage,degree=2)+distance,data=AudiA4)
summary(fit) # interesting to note distance coef

par(mfrow=c(2,2))
plot(fit)
# perhaps a little evidence of heteroskedasticity, and errors don't look normal

##
## Pairwise bootstrap
##

B <- 1000

beta.boot.pair <- matrix(nc=6,nr=B)

set.seed(110)
for(b in 1:B){
  beta.boot.pair[b,] <- lm(price~poly(year,degree=2)+poly(mileage,degree=2)+distance,
    data=AudiA4[sample(1:dim(AudiA4)[1],size=dim(AudiA4)[1],replace=TRUE),])$coef
}

par(mfrow=c(2,3))
for(i in 1:6){
  hist(beta.boot.pair[,i],main=names(fit$coef)[i],breaks=50)
  abline(v=fit$coef[i],col="red")
}

dev.off()
pairs(beta.boot.pair)

##
## Even though there's heteroskedasticity, let's bootstrap residuals anyway
##

beta.boot.resid <- matrix(nc=6,nr=B)

set.seed(110)
for(b in 1:B){
  resid.boot <- sample(fit$resid,size=dim(AudiA4)[1],replace=TRUE)
  AudiA4.boot <- AudiA4
  AudiA4.boot$price <- fit$fitted + resid.boot
  beta.boot.resid[b,] <- lm(price~poly(year,degree=2)+poly(mileage,degree=2)+distance,
    data=AudiA4.boot)$coef
}

par(mfrow=c(2,3))
for(i in 1:6){
  hist(beta.boot.resid[,i],main=names(fit$coef)[i],breaks=50)
  abline(v=fit$coef[i],col="red")
}

## Compare pairwise and residual bootstrap distributions
par(mfrow=c(2,3))
for(i in 1:6){
  plot(density(beta.boot.resid[,i]),main=names(fit$coef)[i],xlim=range(beta.boot.pair[,i]))
  lines(density(beta.boot.pair[,i]),lty=2)
  abline(v=fit$coef[i],col="red")
}

# standard errors
mat <- cbind(summary(fit)$coef[,2],apply(beta.boot.resid,2,sd),apply(beta.boot.pair,2,sd))
colnames(mat) <- c("OLS","Bootstrap residuals","Bootstrap pairs")
mat

# what the heck is going on with the intercept?
# the errors are clearly heteroskedastic, and so bootstrapping residuals
# breaks dependence between eps_i and (x_i,y_i) -- our uncertainty from
# the paired bootstrap is more honest, and OLS/residual bootstraps are too confident!

##
## The wild bootstrap
## y_i^* = hat(y_i) + v_i*hat(eps_i) where v_i is a mean zero, variance one random variable
## That is, each observation is perturbed by a scaled version of its *own* residual
## More robust against heteroskedasticity
##

beta.boot.wild.norm <- beta.boot.wild.mammen <- matrix(nc=6,nr=B)

# using v_i ~ N(0,1)
set.seed(992)
for(b in 1:B){
  resid.boot <- rnorm(n=length(fit$resid),mean=0,sd=1) * fit$resid
  AudiA4.boot <- AudiA4
  AudiA4.boot$price <- fit$fitted + resid.boot
  beta.boot.wild.norm[b,] <- lm(price~poly(year,degree=2)+poly(mileage,degree=2)+distance,
    data=AudiA4.boot)$coef
}

# using v_i distributed according to Mammen (1993)
set.seed(382)
for(b in 1:B){
  v <- rbinom(n=length(fit$resid),size=1,prob=(sqrt(5)+1)/(2*sqrt(5)))
  v[v==1] <- -(sqrt(5)-1)/2
  v[v==0] <- (sqrt(5)+1)/2
  resid.boot <- v * fit$resid
  AudiA4.boot <- AudiA4
  AudiA4.boot$price <- fit$fitted + resid.boot
  beta.boot.wild.mammen[b,] <- lm(price~poly(year,degree=2)+poly(mileage,degree=2)+distance,
    data=AudiA4.boot)$coef
}

# standard errors
mat <- cbind(summary(fit)$coef[,2],apply(beta.boot.resid,2,sd),apply(beta.boot.pair,2,sd),
  apply(beta.boot.wild.norm,2,sd),apply(beta.boot.wild.mammen,2,sd))
colnames(mat) <- c("OLS","Bootstrap residuals","Bootstrap pairs","Wild bootstrap Normal",
  "Wild bootstrap Mammen")
mat
# it has also been shown that the paired bootstrap is sensitive to high leverage
# points, while the wild bootstrap is less so

####################################################################################################
## Using the boot library for logistic regression
####################################################################################################

rm(list=ls())

library(boot)

load("~/Documents/Classes/Fall2023/5610/data/spam.RData")

fit <- glm(spam~capAvg+punc_pound+punc_semiColon,data=dat,family=binomial)
summary(fit)

## Bootstrap it!
coef.get <- function(data,index){
  return(glm(spam~capAvg+punc_pound+punc_semiColon,data=data[index,],
    family=binomial)$coef)
}

coef.get(data=dat,index=1:dim(dat)[1])

set.seed(281)
boot(data=dat,statistic=coef.get,R=1000)
# compare to
summary(fit)
