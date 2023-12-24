################################################################################################
## The lasso
################################################################################################

library(glmnet)

load("~/Documents/Classes/Fall2023/5610/data/hiv.RData")

x <- hiv.train$x
y <- hiv.train$y

##
## lasso via glmnet
##

lasso.mod <- glmnet(x,y,alpha=1,lambda=10^seq(-2,2,length.out=100)) # alpha=1 => lasso

plot(lasso.mod,xvar="lambda")

## Predict on test data for given lambda, compare to null model
lasso.pred <- predict(lasso.mod,s=0.1,newx=hiv.test$x)

mean( (mean(hiv.test$y) - hiv.test$y)^2 ) # null model (beta_0 is only term)
mean( (lasso.pred - hiv.test$y)^2 ) # lasso predictions

# OLS MSE
dat <- data.frame(y,x)
ols <- lm(y~.,data=dat)
ols.pred <- predict(ols,newdata=data.frame(hiv.test$x))
mse.ols <- mean( (ols.pred - hiv.test$y)^2 )
mse.ols

##
## Choose lambda by cross-validation
##

## Lasso
set.seed(3)
cv.out <- cv.glmnet(x,y,alpha=1,lambda=seq(0.001,1,length.out=1000))
plot(cv.out)
lambda.lasso <- cv.out$lambda.min
lambda.lasso

## Ridge regression
set.seed(3)
cv.out <- cv.glmnet(x,y,alpha=0,lambda=seq(0.001,1,length.out=1000))
plot(cv.out)
lambda.ridge <- cv.out$lambda.min
lambda.ridge

## Predict on test data with optimal lambda
lasso.pred <- predict(lasso.mod,s=lambda.lasso,newx=hiv.test$x)
mse.lasso <- mean( (lasso.pred - hiv.test$y)^2 )
ridge.pred <- predict(glmnet(x,y,alpha=0,lambda=lambda.ridge),newx=hiv.test$x)
mse.ridge <- mean( (ridge.pred - hiv.test$y)^2 )

mse.ols
mse.ridge
mse.lasso
1 - mse.lasso/mse.ols # % reduction in MSE over OLS
1 - mse.lasso/mse.ridge # % reduction in MSE over ridge regression

## Which coefficients are present in final model?
predict(lasso.mod,s=lambda.lasso,type="coefficient")

