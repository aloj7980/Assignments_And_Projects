################################################################################################
## Ridge Regression as a solution to collinearity
################################################################################################

library(MASS)

##
## Generating some data with collinear covariates
##

set.seed(923)
n <- 100 # sample size

x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x3c <- 10*x1 + x3 # New variable
y <- x1 + x2 + rnorm(n)

dat <- data.frame(y=y,x1=x1,x2=x2,x3=x3)
datc <- data.frame(y=y,x1=x1,x2=x2,x3=x3c) # note x3 is now correlated with x1,x2!

## OLS fit of 3-variable model using independent x3
pairs(dat)
fit <- lm(y~.,data=dat)
summary(fit)

## OLS fit of 3-variable model using correlated x3
pairs(datc)
fit2 <- lm(y~.,data=datc)
summary(fit2)

## Ridge regression using independent variables
ridge <- lm.ridge(y~., data=dat, lambda=seq(0, 0.1, 0.001))
plot(ridge)

## Ridge regression using correlated variables
ridge2 <- lm.ridge(y~., data=datc, lambda=seq(0, 0.1, 0.001))
plot(ridge2)
# which model minimizes the GCV?
select(ridge2)

## Selection of lambda is at endpoint, extend endpoint and try again
ridge2 <- lm.ridge(y~., data=datc, lambda=seq(0, 20, 0.001))
plot(ridge2)
abline(h=0)
# which model minimizes the GCV?
select(ridge2)

## Final model uses lambda=13.588
ridge.final <- lm.ridge(y~., data=datc, lambda=13.588)
ridge.final

##
## Do some predictions using various models (there is no predict() method for "ridgelm" objects)
##

seqx <- seq(0.05,0.95,0.1)
test <- expand.grid(x1=seqx,x2=seqx,x3=seqx)
test$x3c <- 10*test$x1 + test$x3

mu <- test$x1 + test$x2 # true mean at new features

pred.fit <- predict(fit,newdata=test)   # y ~ X1 + X2 + X3
pred.fit2 <- predict(fit2,newdata=test) # y ~ X1 + X2 + X3c
pred.ridge <- coef(ridge.final)[1] + coef(ridge.final)[2]*test[,1] + 
  coef(ridge.final)[3]*test[,2] + coef(ridge.final)[4]*test[,4]

## Mean squared prediction errors
mean( (pred.fit - mu)^2 ) # OLS with uncorrelated covariates
mean( (pred.fit2 - mu)^2 ) # OLS with collinear covariates
mean( (pred.ridge - mu)^2 ) # Ridge with collinear covariates

##
## What happens to coefficients as lambda=>infty?
##

plot(lm.ridge(y~., data=datc, lambda=seq(0, 10000, 1)))
abline(h=0)

################################################################################################
## HIV data (Rhee et al. 2006)
## A major issue with antiretroviral drugs is the mutation of the virus’ genes.
## Because of its high rate of replication (10^9 to 10^10 virus per person per day) and error-prone
## polymerase, HIV can easily develop mutations that alter susceptibility to antiretroviral drugs.
## The emergence of resistance to one or more antiretroviral drugs is one of the more common reasons
## for therapeutic failure in the treatment of HIV.
##
## In these data from the paper, a sample of in vitro HIV viruses were grown and exposed to a
## particular antiretroviral therapy. The susceptibility of the virus to treatment and the
## number of genetic mutations of each virus were recorded.
################################################################################################

rm(list=ls())

library(glmnet)

load("~/Documents/Classes/Fall2023/5610/data/hiv.RData")

ls()

names(hiv.train)

x <- hiv.train$x
y <- hiv.train$y
# n, p?
# x is 0/1, 1 = mutation in particular gene, 0 = no mutation
# The response is the log transformed susceptibility of a virus to the considered treatment,
# with large values indicating the virus is resistant (that is, not susceptible).

table(x)
hist(y)

##
## Ridge regression via glmnet
##

10^seq(-2,2,length.out=100) # values for lambda
ridge.mod <- glmnet(x,y,alpha=0,lambda=10^seq(-2,2,length.out=100)) # alpha=0 => ridge

dim(coef(ridge.mod))
# note glmnet recalculates beta0.hat on the fly as y.bar - y.hat(at current beta.hats)
plot(coef(ridge.mod)[20,]~ridge.mod$lambda,type="b")

plot(ridge.mod,xvar="lambda")

## What if we wanted coefficients at lambda = 1?
predict(ridge.mod,s=1,type="coefficients")[1:10,]

## Predict on test data, compare to null model
ridge.pred <- predict(ridge.mod,s=0.1,newx=hiv.test$x)
mean( (ridge.pred - hiv.test$y)^2 )

# compare this against prediction based on null model
mean( (mean(hiv.test$y) - hiv.test$y)^2 )

# OLS MSE
dat <- data.frame(y,x)
ols <- lm(y~.,data=dat)
summary(ols)
ols.pred <- predict(ols,newdata=data.frame(hiv.test$x))
mse.ols <- mean( (ols.pred - hiv.test$y)^2 )
mse.ols

##
## Choose lambda by cross-validation
##

set.seed(3)
cv.out <- cv.glmnet(x,y,alpha=0,lambda=seq(0.01,1,length.out=1000)) # 10-fold CV default
plot(cv.out)
lambda <- cv.out$lambda.min
lambda

## Find coefficients for optimal lambda and compare to OLS estimates
ridge.coef <- predict(ridge.mod,s=lambda,type="coefficient")
plot(ols$coef,c(as.matrix(ridge.coef)),xlab="OLS beta",ylab="Ridge beta")
abline(0,1)
# note some are missing since OLS can't deal with collinear features!

## Predict on test data with optimal lambda
ridge.pred <- predict(ridge.mod,s=lambda,newx=hiv.test$x)
mse.ridge <- mean( (ridge.pred - hiv.test$y)^2 )

mse.ols
mse.ridge
1 - mse.ridge/mse.ols # reduction in predictive MSE

