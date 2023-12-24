################################################################################################
## Simple linear regression: y = beta0 + beta1*x + eps
################################################################################################

set.seed(371)

##
## Generate a dataset
##

## Fix features
n <- 20 # sample size
x <- sort(runif(n,min=0,max=10))
beta0 <- -3
beta1 <- 0.5

## Simulate some data
y <- beta0 + beta1 * x + rnorm(n)
plot(y~x)
rug(x) # locations of features

## Add *true* regression line to plot:
Ey <- beta0 + beta1 * x
lines(Ey~x)

##
## Generate a bunch of possible datasets
##

y.many <- matrix(nrow=n,ncol=6)
for(i in 1:6){
  y.many[,i] <- beta0 + beta1 * x + rnorm(n)
}

## Plot them all
par(mfrow=c(2,3))
for(i in 1:6){
  plot(y.many[,i]~x,ylim=range(y.many),ylab="y")
  lines(Ey~x)
}

##
## Residual sum of squares as a function of beta0 and beta1
##

beta0.long <- seq(-5,5,length.out=100)
beta1.long <- seq(-1,1,length.out=100)

betas <- as.matrix(expand.grid(beta0.long,beta1.long))
dim(betas)

RSS <- NULL
for(i in 1:dim(betas)[1]){
  RSS[i] <- sum( (y - (betas[i,1] + betas[i,2]*x))^2 )
}
RSS <- matrix(RSS,nc=100,nr=100)

## Plot RSS
dev.off()
image(x=beta0.long,y=beta1.long,z=RSS,col=rainbow(256),xlab=expression(beta[0]),ylab=expression(beta[1]))
contour(x=beta0.long,y=beta1.long,z=RSS,add=TRUE)

# add truth to plot
abline(v=beta0)
abline(h=beta1)

##
## Ordinary least squares
##

hat.beta1 <- sum( (x - mean(x)) * (y - mean(y)) ) / sum( (x - mean(x))^2 )
hat.beta0 <- mean(y) - hat.beta1 * mean(x)

hat.beta0
hat.beta1
points(x=hat.beta0,y=hat.beta1,pch=19)

## Using built-in R function
fit <- lm(y~x)
fit
class(fit)
names(fit)

summary(fit)
names(summary(fit))
summary(fit)$coef

## Estimate variance
hat.sigma2 <- sum( (y - (hat.beta0 + hat.beta1 * x))^2 ) / (n-2) # unbiased version
hat.sigma2
summary(fit)$sigma^2

## Plot best fit and truth
plot(y~x)
lines(Ey~x)
lines(fit$fitted~x,col="red")
legend(x="topleft",legend=c("Truth","OLS fit"),col=c("black","red"),lty=c(1,1))

##
## Plot possible best fits for all 6 datasets
##

plot(Ey~x,lwd=4,type="l")
for(i in 1:6){
  fit.temp <- lm(y.many[,i]~x)
  lines(fit.temp$fitted~x,col=i)
}
# this is important next -- when we talk about (beta0.hat + beta1.hat*x_0) for
# a new feature x_0, there is uncertainty in this line!

################################################################################################
## Same thing with the College dataset
################################################################################################

library(ISLR2)

data(College)
?College

plot(Grad.Rate~Outstate,data=College,pch=19,cex=0.1)

## Subsets generate different fitted models
set.seed(8)
sets <- sample(1:6,size=dim(College)[1],replace=TRUE)
sets

par(mfrow=c(2,3))
for(i in 1:6){
  plot(Grad.Rate~Outstate,data=College[sets==i,],xlim=range(College$Outstate),
    ylim=range(College$Grad.Rate))
  abline(lm(Grad.Rate~Outstate,data=College[sets==i,]),col="red")
  print(lm(Grad.Rate~Outstate,data=College[sets==i,])$coef)
}

dev.new()

plot(Grad.Rate~Outstate,data=College,pch=19,cex=0.1)
for(i in 1:6){
  abline(lm(Grad.Rate~Outstate,data=College[sets==i,]),col=i)
}
abline(lm(Grad.Rate~Outstate,data=College),lwd=2,lty=2)

# Interpret hat(beta)
