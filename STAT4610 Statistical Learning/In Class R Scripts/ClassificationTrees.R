################################################################################################
## Classification Trees
################################################################################################

library(rpart)
library(ranger)

load("~/Documents/Classes/STAT5610data/spam.RData")

ls()

str(dat)

dat$spam <- as.factor(dat$spam)

##
## Logistic regression on everything
##

fit.lm <- glm(spam~.,data=dat,family=binomial,subset=train)
summary(fit.lm)

## Gini index
pred.lm <- predict(fit.lm,newdata=dat[!train,],type="response")
sum(pred.lm*(1-pred.lm))

##
## Single classification tree
##

fit.tree <- rpart(spam~.,data=dat,subset=train)

summary(fit.tree)

plot(fit.tree)
text(fit.tree)

## Gini index
pred.tree <- predict(fit.tree,newdata=dat[!train,])[,2]
sum(pred.tree*(1-pred.tree))
# compare to
sum(pred.lm*(1-pred.lm))

##
## Compare model fits with ROC curves
##

library(pROC)

r.lm <- roc(dat$spam[!train]~pred.lm)
r.tree <- roc(dat$spam[!train]~pred.tree)

plot(r.lm) # great -- adding a lot of true positive classification without sacrificing FPR
plot(r.tree,add=TRUE,col="red")

##
## Bagging
##

set.seed(430)
pred.boot <- ranger(spam~.,data=dat[train,],mtry=dim(dat)[2]-1,num.trees=500)
pred.bag <- predict(pred.boot,data=dat[!train,])$predictions

pred.lm[pred.lm > 0.5] <- 1
pred.lm[pred.lm < 0.5] <- 0

## Percent correctly specified
mean( ((dat$spam[!train]==1) == pred.lm) )
mean( ((dat$spam[!train]==1) == (as.numeric(pred.bag)-1)) )

##
## Variable importances
##

set.seed(58)
# Nembrini et al (2018)
pred.boot <- ranger(spam~.,data=dat[train,],probability=TRUE,importance="impurity_corrected",
  mtry=dim(dat)[2]-1,num.trees=500)
cbind(sort(importance(pred.boot)))

# Breiman (2001)
pred.boot2 <- ranger(spam~.,data=dat[train,],probability=TRUE,importance="permutation",
  mtry=dim(dat)[2]-1,num.trees=500)
cbind(sort(importance(pred.boot2)))

plot(importance(pred.boot),importance(pred.boot2),type="n")
text(importance(pred.boot),importance(pred.boot2),labels=names(importance(pred.boot)),cex=0.5)
