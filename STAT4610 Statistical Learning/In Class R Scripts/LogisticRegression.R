################################################################################################
## A simple spam classifier: logistic regression
################################################################################################
## The data consist of:
#
# 48 continuous real [0,100] attributes of type WORD 
# = percentage of words in the e-mail that match WORD,
# i.e. 100 * (number of times the WORD appears in the e-mail) / 
# total number of words in e-mail.  A "word" in this case is any 
# string of alphanumeric characters bounded by non-alphanumeric 
# characters or end-of-string.
#
# 6 continuous real [0,100] attributes of type punc_CHAR
# = percentage of characters in the e-mail that match CHAR,
# i.e. 100 * (number of CHAR occurences) / total characters in e-mail
#
# 1 continuous real [1,...] attribute of type capAvg
# = average length of uninterrupted sequences of capital letters
#
# 1 continuous integer [1,...] attribute of type capLong
# = length of longest uninterrupted sequence of capital letters
#
# 1 continuous integer [1,...] attribute of type capTot
# = sum of length of uninterrupted sequences of capital letters
# = total number of capital letters in the e-mail
#
# 1 nominal {0,1} class attribute of type spam
# = denotes whether the e-mail was considered spam (1) or not (0), 
# i.e. unsolicited commercial e-mail.  
################################################################################################

load("~/Documents/Classes/Fall2023/5610/data/spam.RData")

ls()

names(dat)

str(dat)

dim(dat)
length(train)

table(train)

## Take a look at some data
table(dat$spam)
mean(dat$spam) # 39.4% of training data are spam

## Length of longest uninterrupted string of capital letters
plot(spam~capLong,data=dat)
plot(spam~log(capLong),data=dat)
boxplot(capLong~spam,data=dat)
boxplot(capLong~spam,data=dat,outline=FALSE)

## Frequency of the word "business"
plot(spam~business,data=dat)
plot(spam~log(business),data=dat)
boxplot(business~spam,data=dat)
boxplot(business~spam,data=dat,outline=FALSE)

## Set up testing data
spam.test <- dat[!train,]$spam

##
## Logistic regression on capLong
##

fit <- glm(spam~capLong,data=dat,family=binomial,subset=train) # family = binomial => logistic
summary(fit)

## Check fitted values
yhat <- fit$fitted.values
y <- dat$spam[train]
boxplot(yhat~y)

## Check deviance manually
2*(sum(y*log(1/yhat)) + sum((1-y)*log(1/(1-yhat))))
summary(fit)$deviance
rm(y,yhat)
# degrees of freedom: same as multiple regression
sum(train) - 2

## Deviance residuals
dev <- resid(fit)
plot(dev)
which.max(abs(dev))
dat[2015,] # or dat[train,][1893,]
dat[2015,c("spam","capLong")]

## Predicted probabilities
fit.probs <- predict(fit,newdata=dat[!train,],type="response")
cbind(dat[!train,]$capLong,fit.probs)[1:10,]

plot(fit.probs~dat[!train,]$capLong)

## Classification rule
fit.pred <- rep(0,sum(!train))
fit.pred[fit.probs > 0.5] <- 1

## Confusion matrix
table(fit.pred,spam.test)
# proportion of correct predictions
mean(fit.pred == spam.test)
# error rate
mean(fit.pred != spam.test)
# True positive rate
mean(fit.pred[spam.test == 1])
# False positive rate
mean(fit.pred[spam.test == 0])

## Predict for some incoming e-mails
predict(fit,newdata=data.frame(capLong=c(4,10,20,50,51)),type="response")

##
## ROC curve for first fit
##

plot(c(0,1),c(0,1),xlim=c(0,1),ylim=c(0,1),type="l",xlab="FPR",ylab="TPR")

## Function that will give ROC coordinates (FPR,TPR) for a given classification cutoff p
ROC.point <- function(p){
  cr <- rep(0,length(spam.test))
  cr[fit.probs > p] <- 1
  TPR <- sum(cr == 1 & spam.test == 1) / sum(spam.test == 1)
  FPR <- sum(cr == 1 & spam.test == 0) / sum(spam.test == 0)
  return(c(FPR,TPR))
}

ROC.point(0.5)

p <- 0.5
points(ROC.point(p)[1],ROC.point(p)[2])

p <- 0.25
ROC.point(p)
points(ROC.point(p)[1],ROC.point(p)[2])
# Does this make sense?  Lower cutoff => more e-mails classified as spam, rightly or wrongly

for(p in seq(0,1,by=0.01)){
  text(ROC.point(p)[1],ROC.point(p)[2],labels=p,cex=0.5)
  #points(ROC.point(p)[1],ROC.point(p)[2])
}
# maybe a classification threhold of 40% reasonable?

##
## Logistic regression on internet
##

fit2 <- glm(spam~internet,data=dat,family=binomial,subset=train)
summary(fit2)

fit2.probs <- predict(fit2,newdata=dat[!train,],type="response")

plot(fit2.probs~dat[!train,]$internet)

fit2.pred <- rep(0,length(spam.test))
fit2.pred[fit2.probs > 0.5] <- 1

table(fit2.pred,spam.test)
# proportion of correct predictions
mean(fit2.pred == spam.test)
# error rate
mean(fit2.pred != spam.test)
# True positive rate
mean(fit2.pred[spam.test == 1])
# False positive rate
mean(fit2.pred[spam.test == 0])

## Predict for some incoming e-mails
predict(fit2,newdata=data.frame(internet=c(0,1,10)),type="response")

##
## Compare model fits with ROC curves and Brier scores
##

library(pROC)

r.capLong <- roc(spam.test~fit.probs)
r.internet <- roc(spam.test~fit2.probs)

plot(r.capLong) # great -- adding a lot of true positive classification without sacrificing FPR
plot(r.internet,add=TRUE,col="red")

## Brier scores
mean( (dat[!train,]$spam - fit.probs)^2 ) # caplong model
mean( (dat[!train,]$spam - fit2.probs)^2 ) # internet model
