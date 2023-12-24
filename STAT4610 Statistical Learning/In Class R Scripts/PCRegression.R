################################################################################################
## Principal components as a dimension reduction technique
################################################################################################

library(e1071)

##
## Wet bulb temperature data
##

load("~/Documents/Classes/STAT5610data/ScheuererData/predictors.Rdata")

table(ptype)

dim(Twb.prof)

matplot(t(Twb.prof[1:50,]),xlab="Altitude",ylab="Wet bulb temperature")

##
## Some exploratory analysis
##

n <- 1000

set.seed(2820)

train <- sample(1:length(ptype),size=n,replace=FALSE)

twb.full <- Twb.prof[train,] # all temperatures
pt <- ptype[train]

##
## Only deal with RA/SN
##

twb.full <- twb.full[pt == "RA" | pt == "SN",]
pt <- pt[pt == "RA" | pt == "SN"]

##
## Radial support vector machine on all data
##

dat.full <- data.frame(X=twb.full,y=as.factor(pt))

tune.out <- tune(svm,y~.,data=dat.full,kernel="radial",
  ranges=list(cost=seq(0.01,5,length.out=50)))
fit.full <- tune.out$best.model

fit.full

## Confusion matrix
table(fitted=fit.full$fitted,true=pt)

##
## Principal components on training data
##

pca <- prcomp(twb.full) # automatically centers, does *not* scale
# columns of pca$rotation are  eigenvectors (normalized to unit variance)
matplot(pca$rotation[,1:3],type="l")

# rows of pca$x contain the principal components, aka scores
pca$x[1:5,1:6]

# perecent of variance explained 
cumsum(pca$sdev^2)/sum(pca$sdev^2)

## SVM on subset of scores
plot(pca$x[,1:2])
points(pca$x[pt=="RA",1:2],col="blue")
points(pca$x[pt=="SN",1:2],col="grey")

dat.pca <- data.frame(X=pca$x[,1:2],y=as.factor(pt))

tune.out <- tune(svm,y~.,data=dat.pca,kernel="radial",
  ranges=list(cost=seq(0.01,5,length.out=50)))
fit.pca <- tune.out$best.model

## Confusion matrices
table(fitted=fit.full$fitted,true=pt) # 31 predictors
table(fitted=fit.pca$fitted,true=pt) # 2 predictors

