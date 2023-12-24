################################################################################################
## Principal component analysis
################################################################################################

##
## SVD, eigendecomposition and principal component analysis
##

n <- 5
set.seed(7)
x1 <- rnorm(n)
x2 <- 2*x1 + rnorm(n,sd=0.5)

x1 <- x1 - mean(x1)
x2 <- x2 - mean(x2)

M <- cbind(x1,x2)

plot(M)
abline(h=0);abline(v=0)

## Eigenvalues of (M^t M) and (M M^t)
M.2 <- t(M) %*% M
M.3 <- M %*% t(M)
eig.2 <- eigen(M.2)
eig.3 <- eigen(M.3)
# note these are different matrices:
M.2
M.3
# but they have the same eigenvalues (up to some zeros):
eig.2$val
eig.3$val

## SVD of M
svd <- svd(M)

## Principal component function
pc <- prcomp(M,center=FALSE,scale.=FALSE)
names(pc)
# According to prcomp help file:
# sdev = sqrt of eigenvalues (same as class)
# rotation = "loadings" = eigenvectors (not the same as "loadings" from class)
# x = "data multiplied by rotation" (principal components, or scores, from class)

## Compare diagonal matrix entries amongst different methods
round(eig.2$val,digits=10)
round(eig.3$val,digits=10)

svd$d
svd$d^2

pc$sdev
pc$sdev^2 * (5-1)

## Compare eigenvectors, left/right singular vectors, principal component vectors
# Right singular vectors
eig.2$vector
svd$v
pc$rotation
# Left singular vectors
eig.3$vectors
svd$u
pc$x
cbind(pc$x[,1] / (sqrt(n-1)*pc$sdev[1]), pc$x[,2] / (sqrt(n-1)*pc$sdev[2]))

## Writing any given data vector as a weighted sum of principal components
M[4,]
pc$x[4,1]*pc$rotation[,1] + pc$x[4,2]*pc$rotation[,2]
# = z_41 a_1 + z_42 a_2 in class notation

## Note pc$x contains the scores -- i.e., projection of data onto eigenvectors
M %*% pc$rotation
pc$x
# (in particular, note pc$x[,1] is "bigger" than pc$x[,2])

## Eigenvalues of hat(Sigma)_x are sample variances of the scores
eigen( (t(M) %*% M)/(5-1) )$val
apply(pc$x^2,2,sum)/(5-1)
pc$sdev^2

## pc$rotation contains the *eigenvectors*
## pc$rotation*eigenvalues contains the *loadings*
# sanity check: pc$rotation columns should be orthonormal
t(pc$rot) %*% pc$rot

rm(list=ls())

##
## PCA geometry
##

set.seed(7)
x1 <- rnorm(100)
x2 <- 2*x1 + rnorm(100,sd=0.2)

x1 <- x1 - mean(x1)
x2 <- x2 - mean(x2)

dat <- data.frame(x1,x2)

plot(dat,asp=1)

pc <- prcomp(dat,center=FALSE)
pc

# X is nx2, Z is nx2 and A is 2x2
pc$sdev # standard deviations of principal components sqrt(diag(Var(Z)))
pc$rotation # eigenvectors (transpose is A)
pc$x # principal components (Z is nx2, each row has two principal components for ith data point x_i)
# sanity check
head(pc$x)
head(t(t(pc$rotation) %*% t(as.matrix(dat)))) # A %*% t(X) = t(Z)

summary(pc)

par(mfrow=c(1,2))
plot(dat)
arrows(0,0,pc$rot[1,1],pc$rot[2,1],lwd=2,length=0.1,col="red")
arrows(0,0,pc$rot[1,2],pc$rot[2,2],lwd=2,length=0.1,col="red")
plot(dat,asp=1)
arrows(0,0,pc$rot[1,1],pc$rot[2,1],lwd=2,length=0.1,col="red")
arrows(0,0,pc$rot[1,2],pc$rot[2,2],lwd=2,length=0.1,col="red")
# or add loadings to the plot:
arrows(0,0,pc$rot[1,1]*pc$sd[1]^2,pc$rot[2,1]*pc$sd[1]^2,lwd=2,length=0.1,col="purple")
arrows(0,0,pc$rot[1,2]*pc$sd[2]^2,pc$rot[2,2]*pc$sd[2]^2,lwd=2,length=0.1,col="purple")

## Principal components are a type of perpendicular regression
set.seed(7)
x1 <- rnorm(100)
x2 <- 2*x1 + rnorm(100,sd=1)

x1 <- scale(x1)
x2 <- scale(x2)

dat <- data.frame(x1,x2)

pc <- prcomp(dat,center=FALSE)
lm1 <- lm(x2~x1,data=dat)
lm2 <- lm(x1~x2,data=dat)

plot(dat,asp=1)
abline(a=0,b=lm1$coef[2])
abline(a=0,b=1/lm2$coef[2])
segments(x0=0,y0=0,x1=10*pc$rot[1,1],y1=10*pc$rot[2,1],col="red")
segments(x0=0,y0=0,x1=-10*pc$rot[1,1],y1=-10*pc$rot[2,1],col="red")

rm(list=ls())

##
## PCA in 3d
##

library(rgl)
library(MASS)

set.seed(3)
n <- 200
Sigma <- diag(c(3,1,1))
Sigma[1,2] <- Sigma[2,1] <- 0
Sigma[1,3] <- Sigma[3,1] <- sqrt(3) * 0.99
Sigma[2,3] <- Sigma[3,2] <- 0
Sigma

sim <- mvrnorm(n=n,mu=c(0,0,0),Sigma=Sigma)

plot3d(sim)

pc <- prcomp(sim)
summary(pc) # almost entirely explained by first two PCs

rm(list=ls())

##
## To check for outliers
##

set.seed(8)
dat <- matrix(rnorm(10*100),nc=10,nr=100)
dim(dat)
dat[5,8] <- 10 # 5th data point has outlier
pc <- prcomp(dat)

plot(pc$x[,1:2],type="n")
text(pc$x[,1:2])

rm(list=ls())

##
## Wet bulb temperature data
##

load("~/Documents/Classes/STAT5610data/ScheuererData/predictors.Rdata")

dim(Twb.prof)
length(station.ind)
length(date.ind)
plot(cbind(lon,lat))
# Where/when is the first row of data from?
stations[station.ind[1]]
dates[date.ind[1]]
Twb.prof[1,]

matplot(t(Twb.prof[1:50,]),xlab="Altitude",ylab="Wet bulb temperature",type="l")

out <- prcomp(Twb.prof) # automatically centers, does *not* scale

## Perecent of variance explained 
cumsum(out$sdev^2)/sum(out$sdev^2)

## What do the eigenvectors look like?
par(mfrow=c(3,3))
for(i in 1:9){
  plot(out$rotation[,i],type="l",ylim=c(-0.5,0.5))
  # note these are really just vectors that we're ``connecting the lines'' with
  points(out$rotation[,i])
}
dev.off()
matplot(out$rotation[,1:3],type="l") # 99.9% of variance explained

# empirical standard deviations of the principal components
apply(out$x,2,sd)
out$sdev

################################################################################################
## CU FCQ data
################################################################################################

rm(list=ls())

dat <- read.csv("~/Documents/Classes/STAT5610data/CUFCQs/CUFCQs20082015.csv",header=TRUE)

dim(dat)
names(dat)

dat <- dat[,-c(5,6,8:10,12,14,23:29,33:39)]
dat <- dat[complete.cases(dat),]
dat <- dat[!(dat$ASdiv=="AC" | dat$ASdiv==""),]
dat$ASdiv <- factor(dat$ASdiv)

dim(dat)

##
## Biplot example
##

n <- 20
X <- dat[1:n,c(6,9:14)]
names(X)

pca <- prcomp(X)

pca$rot
biplot(pca)
# PriorInterest points in generally a different direction:
#   is not so correlated with the other variables
# the other variables are highly correlated
# Other variables primarly control PC1, while PriorInterest is most effective in PC2
# sanity check:
pca$rot[,1:2]
# arrows are via:
t(t(pca$rotation[,1:2]) * pca$sdev[1:2] * sqrt(n))
points(t(t(pca$rotation[,1:2]) * pca$sdev[1:2] * sqrt(n)),pch=19)
# just a rescaled map of the data in the first two PCs
pca$x["26",1:2]
pca$x["27",1:2]
X["26",]
X["27",]
X["15",]
X["12",]

## biplot takes in scaled principal components and scores
lam <- pca$sdev[1:2] * sqrt(n)

yy <- t(t(pca$rotation[,1:2]) * lam)
xx <- t(t(pca$x[,1:2]) / lam)

biplot(xx,yy) # same as biplot(pca)

##
## PCA on FCQ data
##

X <- dat[,c(6,9:14)]
names(X)

pca <- prcomp(X)

plot(pca)
summary(pca)
# perecent of variance explained 
cumsum(pca$sdev^2)/sum(pca$sdev^2)

# biplot
biplot(pca,cex=0.5)
dat[11706,names(X)]

## Principal component regression
K <- 3
fit <- lm(InstructorOverall~.,data=dat[,c(6,7,9:14)])
fit.pca <- lm(dat$InstructorOverall~pca$x[,1:K])

summary(fit)
summary(fit.pca)
# note first PC is controlled mostly by non-prior-interest variables, sign make sense?
# second PC is primarily controlled by PriorInterest... Compare coef to summary(fit)

## Any clusters apparent based on first two principal components?
levels(dat$ASdiv)
plot(pca$x[,1:2],col=dat$ASdiv)

these <- 1:2
plot(pca$x[,these])
text(x=tapply(pca$x[,these[1]],dat$Subject,mean),y=tapply(pca$x[,these[2]],dat$Subject,mean),
  labels=names(tapply(pca$x[,these[1]],dat$Subject,mean)),col="red",cex=1)

plot(pca$x[,these],pch=19,cex=0.1)
text(x=tapply(pca$x[,these[1]],dat$Instructor,mean),y=tapply(pca$x[,these[2]],dat$Instructor,mean),
  labels=names(tapply(pca$x[,these[1]],dat$Instructor,mean)),col="purple",cex=0.5)
text(x=mean(pca$x[dat$Instructor=="KLEIBER, W PAUL",these[1]]),
  y=mean(pca$x[dat$Instructor=="KLEIBER, W PAUL",these[1]]),labels="me",col="red")

# try these = 2:3 or 6:7

################################################################################################
## IMDB data
################################################################################################

rm(list=ls())

dat <- dat.full <- read.csv("~/Documents/Classes/STAT5610data/IMDB-Movie-Data.csv")

dat <- dat[,c(7,8,9,10,11,12)]
dat <- dat[complete.cases(dat),]
# dat <- scale(dat) # do after this analysis

names(dat)
dim(dat)

##
## PCA
##

## A first PCA
pca <- prcomp(dat)
plot(pca)
pca$rot

# Biplot
biplot(pca)
# Hmm, why would votes be so important?
head(dat)
dat.full["55",]
dat.full["51",]

dat <- dat[,-4] # remove votes

## A second PCA
pca <- prcomp(dat)
plot(pca)
pca$rot

## Biplot
biplot(pca)
dat.full["312",]
dat.full["88",]
dat.full["86",]
dat.full["1000",]

# perecent of variance explained 
cumsum(pca$sdev^2)/sum(pca$sdev^2)

##
## PCA for scaled data
##

dat <- scale(dat)

## A first PCA
pca <- prcomp(dat)
plot(pca)
pca$rot

summary(pca) # only really 4 independent directions of variation... Does that make sense?

# Take a look at the most important variables that define the last eigenvector
pca$rot
# notice sign difference... Getting at movies for which metascore and ratings differ

## Biplot
biplot(pca)
# Movie with an abysmal rating
dat.full["830",]
dat.full["872",]

biplot(pca,choices=c(6,7))
# movies where metascore != ratings
dat.full["735",] # movies where metascore != ratings
dat.full["992",]
# movies where runtime and revenue are opposite
dat.full["51",]
dat.full["312",]
dat.full["89",]
dat.full["268",]

