rm(list=ls())
##############
#Read in the data
##############
tab=read.table('stroke_data_1986.txt',sep='\t',header=TRUE,stringsAsFactors=FALSE)
Ns=67; Ag=6
Y=array(tab[,3],dim=c(Ns,Ag))
n=array(tab[,4],dim=c(Ns,Ag))
county_name=tab[1:Ns,1]
ages=c('35-44','45-54','55-64','65-74','75-84','85+')

if(FALSE){
  write.table(Y,'Y.txt',sep=', ',row.names=FALSE,col.names=FALSE,eol = ",\n",)
  write.table(n,'n.txt',sep=', ',row.names=FALSE,col.names=FALSE,eol = ",\n",)
}
#Set up the prior distributions
lambda0=apply(Y,2,sum)/apply(n,2,sum)
n0=apply(n,2,quantile,.1)
a0=lambda0*n0

#Get the samples from the posterior distribution
nsims=10000
lambda=array(dim=c(Ns,Ag,nsims))
#dimnames(lambda)=list(tab[1:Ns,1],
#                      c('35-44','45-54','55-64','65-74','75-84','85+'),
#                      paste('it_',1:nsims,sep=''))
set.seed(1234)
for(i in 1:Ns){
  for(j in 1:Ag){
    #we're going to multiply these by 100,000
    lambda[i,j,]=rgamma(nsims,Y[i,j]+a0[j],n[i,j]+n0[j]) * 100000
  }
}

####################
#Problem 1
lambdaj=array(dim=c(Ag,nsims))
for(j in 1:Ag){
  lambdaj[j,]=apply(lambda[,j,]*n[,j],2,sum)/sum(n[,j])
}

par(mfrow=c(2,3))
for(j in 1:Ag){
  hist(lambdaj[j,],breaks=100,main=ages[j])
  abline(v=sum(Y[,j])/sum(n[,j]) * 100000,col=2)
}

####################
#Problem 2
prob2=array(dim=c(Ns,Ag))
for(j in 1:Ag){
  prob2[,j]=apply(lambda[,j,]>rep(lambdaj[j,],each=Ns),1,mean)
  cat('Age:',ages[j],'\n')
  print(county_name[prob2[,j]>.95])
}

####################
#Problem 3
urban=(apply(n,1,sum)>50000)
urates=nurates=array(dim=c(Ag,nsims))
for(j in 1:Ag){
  urates[j,]=apply(lambda[urban,j,]*n[urban,j],2,sum)/sum(n[urban,j])
  nurates[j,]=apply(lambda[!urban,j,]*n[!urban,j],2,sum)/sum(n[!urban,j])
}
par(mfrow=c(2,3))
for(j in 1:Ag){
  hist(urates[j,]/nurates[j,],breaks=100,
       main=paste(ages[j],':',round(mean(urates[j,]>nurates[j,]),2)))
  abline(v=1,col=2)
}

####################
#Problem 4
p=apply(n,2,sum)/sum(n)

####################
#Problem 5
lambda.aa=array(dim=c(Ns,nsims))
for(i in 1:Ns){
  lambda.aa[i,]=apply(lambda[i,,]*p,2,sum)
}

####################
#Problem 6
u.aa=apply(lambda.aa[urban,]*apply(n[urban,],1,sum),2,sum)/
     sum(n[urban,])
nu.aa=apply(lambda.aa[!urban,]*apply(n[!urban,],1,sum),2,sum)/
     sum(n[!urban,])
hist(u.aa/nu.aa,breaks=100,
       main=paste(ages[j],':',round(mean(u.aa>nu.aa),2)))

####################
#make maps
load('penn.rdata')
library(maptools)   # general functions for map/spatial manipulation
library(RColorBrewer)
ncols=6
cols=brewer.pal(ncols,'Reds')
lci=apply(lambda,1:2,quantile,c(.5,.025,.975))

par(mfrow=c(2,3),mar=c(0,0,1,0)+.5)
for(j in 1:Ag){
tcuts=quantile(lci[1,,j],(1:(ncols-1))/ncols)
tcolb=array(rep(lci[1,,j],each=ncols-1) > tcuts,
            dim=c(ncols-1,Ns))
tcol =apply(tcolb,2,sum)+1

    plot(penn,col=cols[tcol],border='lightgray')
  title(main=ages[j],cex.main=2)
}


#############################
#Here's the comparison part
#############################
ex1=read.table("1-spatial (coda).txt",header=FALSE,sep='\t')
ex2=read.table("2-mcar (coda).txt",header=FALSE,sep='\t')
nsims2=(dim(ex1)[1]/(Ns*Ag))

elambda1=array(ex1[,2],dim=c(nsims2,Ns,Ag))
par(mfrow=c(2,3))
for(j in 1:Ag){
  plot(elambda1[,51,j],type='l')
}

elambda2=array(ex2[,2],dim=c(nsims2,Ns,Ag))
par(mfrow=c(2,3))
for(j in 1:Ag){
  plot(elambda2[,51,j],type='l')
}

eci1=apply(elambda1,2:3,quantile,c(.5,.025,.975))
eci2=apply(elambda2,2:3,quantile,c(.5,.025,.975))

i=1
par(mfrow=c(2,3))
for(j in 1:Ag){
  matplot(cbind(lambda[i,j,1:nsims2],elambda1[,i,j],elambda2[,i,j]),
          type='l',lty=1) #,x=1:500
}

par(mfrow=c(3,6),mar=c(1,1,1,1)+.1)
for(j in 1:Ag){
  tcuts=quantile(c(lci[1,,j],eci1[1,,j],eci2[1,,j]),(1:(ncols-1))/ncols)
  tcolb=array(rep(lci[1,,j],each=ncols-1) > tcuts,
              dim=c(ncols-1,Ns))
  tcol =apply(tcolb,2,sum)+1

  plot(penn,col=cols[tcol],border='lightgray')
}

for(j in 1:Ag){
  tcuts=quantile(c(lci[1,,j],eci1[1,,j],eci2[1,,j]),(1:(ncols-1))/ncols)
  tcolb=array(rep(eci1[1,,j],each=ncols-1) > tcuts,
              dim=c(ncols-1,Ns))
  tcol =apply(tcolb,2,sum)+1

  plot(penn,col=cols[tcol],border='lightgray')
}

for(j in 1:Ag){
  tcuts=quantile(c(lci[1,,j],eci1[1,,j],eci2[1,,j]),(1:(ncols-1))/ncols)
  tcolb=array(rep(eci2[1,,j],each=ncols-1) > tcuts,
              dim=c(ncols-1,Ns))
  tcol =apply(tcolb,2,sum)+1

  plot(penn,col=cols[tcol],border='lightgray')
}

