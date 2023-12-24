####################################################################################################
## Estimating standard error of a statistic using the bootstrap
####################################################################################################

##
## Nonparametric bootstrap for a case where we know the answer
##

set.seed(932)

n <- 50

x <- rnorm(n=n,mean=0,sd=2)

# bootstrapping the sample mean
B <- 500 # number of bootstrap samples, each of which is size n

boot.mean <- NULL

for(b in 1:B){
  these.in <- sample(1:n,size=n,replace=TRUE)
  boot.mean[b] <- mean(x[these.in])
}

boot.SE <- sqrt( sum( (boot.mean - mean(boot.mean))^2 ) / (B-1) )

# compare to known truth
mat <- cbind(boot.SE,2/sqrt(n))
colnames(mat) <- c("Bootstrap SE estimate","True SD")
round(mat,digits=3)

##
## Nonparametric bootstrap for a more difficult statistic
##

rm(list=ls())

set.seed(97)

n <- 1000 # try again with a small n, like 200

x <- runif(n=n,min=0,max=10)

# estimate the 75% quantile of the distribution from which Xs are being drawn

x.quant <- quantile(x,probs=0.75)

# bootstrapping the quantile
B <- 2000

boot.quant <- NULL

for(b in 1:B){
  these.in <- sample(1:n,size=n,replace=TRUE)
  boot.quant[b] <- quantile(x[these.in],probs=0.75)
}

## "Get" true sampling distribution (not known in practice!!)
x.quant.smp <- NULL
for(i in 1:B){
  x.quant.smp[i] <- quantile(runif(n=n,min=0,max=10),probs=0.75)
}

# compare distributions
par(mfrow=c(1,2))
hist(x.quant.smp,main="True sampling distribution")
abline(v=x.quant)
hist(boot.quant,main="Bootstrapped-estimate of sampling distribution")
abline(v=x.quant)

## Compare estimated standard errors
boot.SE <- sd(boot.quant)
boot.SE
sd(x.quant.smp)
# still pretty decent, note quantiles are difficult

# approximate 95% confidence interval
c(x.quant - 2 * boot.SE, x.quant + 2 * boot.SE)

####################################################################################################
## Estimating bias and standard error of a statistic using the jackknife
####################################################################################################

rm(list=ls())

set.seed(1121)

n <- 40

x <- rexp(n)

x.var <- sum( (x-mean(x))^2 ) / n # instead of unbiased estimate using 1/(n-1)

x.var

# note the true bias is
(n-1)/n - 1 # = -sigma^2 / n

##
## Approximate the bias of x.var with the jackknife
##

JK.vars <- NULL
for(i in 1:n){
  x.tmp <- x[-i]
  JK.vars[i] <- sum( (x.tmp-mean(x.tmp))^2 ) / (n-1) # because removed x_i from sample
}
hist(JK.vars,breaks=20)
abline(v=x.var)

JK.bias <- (n-1) * ( mean(JK.vars) - x.var )

JK.bias

##
## Approximate the standard error with the jackknife
##

JK.se <- sqrt( ((n-1)/n) * sum( (JK.vars - mean(JK.vars))^2 ) )

# an approximate 95% confidence interval for the population variance
c(x.var - 2 * JK.se, x.var + 2 * JK.se)

##
## Should we use the jackknife to bias correct estimators?
##

## For N samples, calculate jackknife estimate of bias to see variability in this estimator
N <- 10000 # number of datasets to draw and calculate jackknife bias
n <- 40 # sample size
true.bias <- (n-1)/n - 1 # = -sigma^2 / n

set.seed(101)

JK.bias <- NULL

for(k in 1:N){
  x <- rexp(n) # if we change the rate we'll have to go back and fix true.bias
  x.var <- sum( (x-mean(x))^2 ) / n # instead of unbiased estimate using 1/(n-1)
  JK.vars <- NULL
  for(i in 1:n){
    x.tmp <- x[-i]
    JK.vars[i] <- sum( (x.tmp-mean(x.tmp))^2 ) / (n-1) # because removed x_i from sample
  }
  JK.bias[k] <- (n-1) * ( mean(JK.vars) - x.var )
}

hist(JK.bias,breaks=100)
abline(v=true.bias,col="red")
# the jackknife estimator of bias is pretty variable -- so bias correcting using it will introduce
# added variability into our estimator, potentially ruining MSE

####################################################################################################
## How confident patients are in communicating in different contexts before and after
## social group therapy
####################################################################################################

rm(list=ls())

library(boot)

dat <- read.csv("~/Documents/Classes/Fall2023/5610/data/CCRSA.rand.csv",header=TRUE)

plot(jitter(dat$pre),jitter(dat$post))
abline(0,1)
hist(dat$post-dat$pre)

## Bootstrap approximation to distribution of mean(post - pre)

# Function that will calculate the mean in post/pre difference for a set of indices
fn <- function(dat.in,ind){
  return(mean(dat.in$post[ind] - dat.in$pre[ind]))
}

fn(dat,ind=1:dim(dat)[1]) # average improvement

set.seed(488)
out <- boot(data=dat,statistic=fn,R=1000)
out
c(5.333 - 2*2.296, 5.333 + 2*2.296) # approx 95% CI

# compare to a standard t-test
t.test(dat$post,dat$pre,paired=TRUE)

## Bootstrap approximation to distribution of mean(post / pre) = average percent improvement
# a function that will calculate the mean in post/pre ratio for a set of indices
fn <- function(dat.in,ind){
  return(mean(dat.in$post[ind] / dat.in$pre[ind],na.rm=TRUE))
}

fn(dat,ind=1:dim(dat)[1]) # average percent improvement

out <- boot(data=dat,statistic=fn,R=1000)
out
c(1.290 - 2*0.119, 1.290 + 2*0.119) # approx 95% CI

####################################################################################################
## X and Y are log investment returns for two financial assets
## Will invest fraction alpha in X, and (1-alpha) in Y, and would like to minimize
## our risk, Var(alpha*X + (1-alpha)*Y)
## Can analytically show that value of alpha that minimizes the risk is
## alpha.hat = (Var(Y) - Cov(X,Y)) / (Var(X) + Var(Y) - 2*Cov(X,Y))
####################################################################################################

rm(list=ls())

library(boot)

## Actual dataset
n <- 100

set.seed(200)

X <- log(runif(n,min=1000,max=5000))
Y <- 0.5*X + rnorm(n,mean=0,sd=0.3)

plot(X,Y)

# function to calculate alpha, given data
alpha.fn <- function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.hat <- alpha.fn(data.frame(X,Y),index=1:n)
alpha.hat

## True sampling distribution of hat{alpha}
true.alpha.hats <- X.smp <- Y.smp <- NULL
B <- 1000
for(i in 1:B){
  X.smp <- log(runif(n,min=1000,max=5000))
  Y.smp <- 0.5*X.smp + rnorm(n,mean=0,sd=0.3)
  true.alpha.hats[i] <- alpha.fn(data.frame(X=X.smp,Y=Y.smp),index=1:n)
}

hist(true.alpha.hats)
abline(v=alpha.hat)

## Bootstrap approximation of the distribution of hat{alpha}
for(i in 1:10){
  print(alpha.fn(data.frame(X=X,Y=Y),index=sample(1:n,n,replace=TRUE)))
}

## Using the boot package to get biases and standard errors
set.seed(2442)
out <- boot(data=data.frame(X,Y),statistic=alpha.fn,R=B)
out
# compare SE against
sd(true.alpha.hats)

## Check estimated distribution of alpha.hat
hist(out$t,xlim=c(0,1))
hist(true.alpha.hats,add=TRUE,col=rgb(red=1,green=0,blue=0,alpha=0.2))

# 95% confidence bounds from estimated, and "true", distributions
quantile(out$t,probs=c(0.025,0.975))
quantile(true.alpha.hats,probs=c(0.025,0.975))
