################################################################################################
## Simulation of 1-D Gaussian processes
################################################################################################

library(fields)

n <- 200
x <- seq(0,5,length.out=n)

##
## Realizations from Gaussian processes
##

set.seed(3872)
## Noise
plot(rnorm(n)~x,type="l",xlab="",ylab="")

##
## Materns, varying smoothness
##

par(mfrow=c(2,2))
nu <- 0.5
for(i in 1:4){
  plot(c(t(chol(Matern(rdist(x),nu=nu))) %*% rnorm(n))~x,type="l",ylab="",xlab="")
}

nu <- 1.5
for(i in 1:4){
  plot(c(t(chol(Matern(rdist(x),nu=nu))) %*% rnorm(n))~x,type="l",ylab="",xlab="")
}

nu <- 2.5
for(i in 1:4){
  plot(c(t(chol(Matern(rdist(x),nu=nu))) %*% rnorm(n))~x,type="l",ylab="",xlab="")
}

##
## Squared exponential, varying length-scale
##

par(mfrow=c(2,2))
for(a in c(0.1,0.5,1,2)){
  plot(c(t(chol(exp(-(rdist(x)/a)^2)+diag(1e-10,n))) %*% rnorm(n))~x,type="l",ylab="",xlab="",
    main=paste("length-scale=",a))
}

##
## Dot product covariance
##

set.seed(3823)
x <- seq(0,100,length.out=n)

k <- function(x,z){
  1 + x*z
}

x.expand <- expand.grid(x,x)

Sigma <- matrix(k(x.expand[,1],x.expand[,2]),n,n)

plot(c(t(chol(Sigma+diag(1e-10,n))) %*% rnorm(n))~x,type="l",ylab="",xlab="")
# makes sense given this is the covariance implied by N(0,1) priors on beta_0 and beta_1 
# in f(x) = beta_0 + beta_1*x
plot(diag(Sigma))
image(Sigma)

################################################################################################
## Simulation of 2-D Gaussian processes f(x1,x2)
################################################################################################

library(fields)

n <- 200
x1 <- seq(0,10,length.out=n)
x2 <- seq(0,10,length.out=n)

x.grd <- expand.grid(x1,x2)

##
## Separable covariance k((x1,x2),(z1,z2)) = k1(x1,z1)*k2(x2,z2)
##

set.seed(7)

## Model 1 (variable 1 has smoothness 0.5, variable 2 has smoothness 2.5)
sim <- t(chol(Matern(rdist(x2),nu=2.5))) %*% matrix(rnorm(n^2),n,n) %*% chol(Matern(rdist(x1),nu=0.5))

par(mfrow=c(1,3))
poly.image(matrix(x.grd[,1],n,n),matrix(x.grd[,2],n,n),sim,xlab="Variable 1",ylab="Variable 2")
abline(v=x1[n/2])
abline(h=x2[n/2])
plot(sim[,n/2],type="l",main="cut of Variable 1")
plot(sim[n/2,],type="l",main="cut of Variable 2")

## Model 2 (variable 1 has smoothness 2.5, variable 2 has smoothness 1.5)
sim <- t(chol(Matern(rdist(x2),nu=1.5))) %*% matrix(rnorm(n^2),n,n) %*% chol(Matern(rdist(x1),nu=2.5))

par(mfrow=c(1,3))
poly.image(matrix(x.grd[,1],n,n),matrix(x.grd[,2],n,n),sim,xlab="Variable 1",ylab="Variable 2")
abline(v=x1[n/2])
abline(h=x2[n/2])
plot(sim[,n/2],type="l",main="cut of Variable 1")
plot(sim[n/2,],type="l",main="cut of Variable 2")

## Model 3 (variables 1 and 2 have squared exponential covariances)
sim <- t(chol(exp(-rdist(x2)^2)+diag(1e-10,n))) %*% matrix(rnorm(n^2),n,n) %*%
  chol(exp(-rdist(x1)^2)+diag(1e-10,n))

par(mfrow=c(1,3))
poly.image(matrix(x.grd[,1],n,n),matrix(x.grd[,2],n,n),sim,xlab="Variable 1",ylab="Variable 2")
abline(v=x1[n/2])
abline(h=x2[n/2])
plot(sim[,n/2],type="l",main="cut of Variable 1")
plot(sim[n/2,],type="l",main="cut of Variable 2")

x11()
par(mfrow=c(2,2))
for(i in 1:4){
  sim <- t(chol(exp(-rdist(x2)^2)+diag(1e-10,n))) %*% matrix(rnorm(n^2),n,n) %*%
    chol(exp(-rdist(x1)^2)+diag(1e-10,n))
  poly.image(matrix(x.grd[,1],n,n),matrix(x.grd[,2],n,n),sim,xlab="Variable 1",ylab="Variable 2")
}

# plots of variable 1 marginally while changing variable 2
# f(.,y) varying y
par(mfrow=c(1,1),ask=TRUE)
for(i in 1:n){
  #poly.image(matrix(x.grd[,1],n,n),matrix(x.grd[,2],n,n),sim,xlab="Variable 1",ylab="Variable 2")
  #abline(h=x2[i])
  plot(sim[,i],main=paste("f(x,",round(x2[i],digits=3),")"),type="l")
}
