#https://wonder.cdc.gov/controller/saved/D140/D34F844
rm(list=ls())
#First we read in the data and define a few things...
stroke=read.table('2016_PA_stroke_total.txt',sep='\t',
                  stringsAsFactors=FALSE,header=TRUE)
Ng=3        #three age groups
alabs=unique(stroke$Age.Group.Code)
Ns=67     #67 counties
clabs=unique(stroke$County)

#Next we organize things a bit...
Y=array(stroke$Deaths,dim=c(Ng,Ns))
n=array(stroke$Population,dim=c(Ng,Ns))

###################
###################
#per part 4, all Y's below 10 have been suppressed
###################
thres=10      #Suppression threshold
dY=!is.na(Y)  #0 for suppressed, 1 for observed
nsupp=apply(!dY,1,sum) #how many suppressed per age
#note: we do not know the true Y's
#      CDC did the suppression, not me

###################
###################
#insert your prior info here
###################
lam0=c(75,250,1000)/100000
total_pop = sum(n)
age_proportions = apply(n, 1, sum) / total_pop
n0= age_proportions * 10000
Y0= n0 * lam0
tau_a2 = 10000  # Variance for beta_0a prior
a0 = 0.001  # For inverse-gamma prior on sigma_a^2
b0 = 0.001  # For inverse-gamma prior on sigma_a^2

###################
###################
#initialize your Gibbs sampler here
nsims=10000
lami=array(dim=c(Ng,Ns,nsims))
for(a in 1:Ng){
  lami[a,,1]= lam0[a]
  Y[a,!dY[a,]]= Y0[a]
  #Note: the preceding line assumes we don't care
  #      what the posterior dist of the missing
  #      Y's looks like -- I'm just plugging the
  #      current guesses directly into my data vector
}
# Initialize Gibbs sampler storage
beta_0a = matrix(0, nrow = Ng, ncol = nsims)  # Store beta_0a samples
sigma_a2 = matrix(1, nrow = Ng, ncol = nsims)  # Store sigma_a^2 samples
z = array(0, dim = c(Ng, Ns, nsims))  # Store latent z_ia samples

for(it in 2:nsims){
  for(a in 1:Ng){
    
    ###################
    # ADDRESS SUPPRESSED Y HERE
    ###################
    
    for(i in 1:Ns){  
      if(!dY[a, i]){ 
        lambda_curr = lami[a, i, it-1]  
        
        # Truncated Poisson sampling via inverse transform
        p_cdf = ppois(9, lambda_curr * n[a, i])  # CDF for Y < 10
        u = runif(1) * p_cdf 
        Y[a, i] = qpois(u, lambda_curr * n[a, i]) 
      }
    }
    
    ###################
    # SAMPLE Z_{ia} USING METROPOLIS
    ###################
    
    for(i in 1:Ns) {
      z_proposed = rnorm(1, mean = z[a, i, it - 1], sd = 0.1)  # Proposal step
      
      # Compute log acceptance ratio
      log_r = Y[a, i] * z_proposed - n[a, i] * exp(beta_0a[a, it - 1] + z_proposed) - 
        (Y[a, i] * z[a, i, it - 1] - n[a, i] * exp(beta_0a[a, it - 1] + z[a, i, it - 1])) -
        (z_proposed^2 / (2 * sigma_a2[a, it - 1])) + (z[a, i, it - 1]^2 / (2 * sigma_a2[a, it - 1]))
      
      # Accept or reject proposal
      if (log(runif(1)) < log_r) {
        z[a, i, it] = z_proposed
      } else {
        z[a, i, it] = z[a, i, it - 1]
      }
    }
    
    ###################
    # SAMPLE BETA_{0a} USING METROPOLIS
    ###################
    
    beta_proposed = rnorm(1, mean = beta_0a[a, it - 1], sd = 0.1)  
    log_r_beta = sum(Y[a, ] * beta_proposed - n[a, ] * exp(beta_proposed + z[a, , it])) - 
      sum(Y[a, ] * beta_0a[a, it - 1] - n[a, ] * exp(beta_0a[a, it - 1] + z[a, , it])) -
      (beta_proposed^2 / (2 * tau_a2)) + (beta_0a[a, it - 1]^2 / (2 * tau_a2))
    
    if (log(runif(1)) < log_r_beta) {
      beta_0a[a, it] = beta_proposed
    } else {
      beta_0a[a, it] = beta_0a[a, it - 1]
    }
    
    ###################
    # SAMPLE SIGMA_A^2 FROM INVERSE-GAMMA
    ###################
    
    shape_new = a0 + Ns / 2
    scale_new = b0 + sum(z[a, , it]^2) / 2
    sigma_a2[a, it] = 1 / rgamma(1, shape = shape_new, rate = scale_new)
    
    ###################
    # ESTIMATE LAMBDA_{ia}
    ###################
    
    for(i in 1:Ns){  
      lami[a, i, it] = exp(beta_0a[a, it] + z[a, i, it])
    }
  }
}

##################
# Compute posterior summaries
##################
# Number of burn-in samples
burnin = 2500

# Plots
par(mfrow = c(2, 3))

plot(beta_0a[1, ], type = 'l', main = expression(beta[0][1]), xlab = "Iteration", ylab = expression(beta[0][1]))
abline(v = burnin, col = "red", lty = 2)  # Mark burn-in threshold
plot(beta_0a[2, ], type = 'l', main = expression(beta[0][2]), xlab = "Iteration", ylab = expression(beta[0][2]))
abline(v = burnin, col = "red", lty = 2)
plot(beta_0a[3, ], type = 'l', main = expression(beta[0][3]), xlab = "Iteration", ylab = expression(beta[0][3]))
abline(v = burnin, col = "red", lty = 2)

hist(beta_0a[1, ], breaks = 100, freq = FALSE, main = expression(beta[0][1]), xlab = expression(beta[0][1]))
hist(beta_0a[2, ], breaks = 100, freq = FALSE, main = expression(beta[0][2]), xlab = expression(beta[0][2]))
hist(beta_0a[3, ], breaks = 100, freq = FALSE, main = expression(beta[0][3]), xlab = expression(beta[0][3]))

par(mfrow = c(2, 3))

# Select a random county for z
random_county = sample(1:Ns, 1)

plot(z[1, random_county, ], type = 'l', main = expression(z[1]), xlab = "Iteration", ylab = expression(z[1]))
abline(v = burnin, col = "red", lty = 2)
plot(z[2, random_county, ], type = 'l', main = expression(z[2]), xlab = "Iteration", ylab = expression(z[2]))
abline(v = burnin, col = "red", lty = 2)
plot(z[3, random_county, ], type = 'l', main = expression(z[3]), xlab = "Iteration", ylab = expression(z[3]))
abline(v = burnin, col = "red", lty = 2)

hist(z[1, random_county, ], breaks = 100, freq = FALSE, main = expression(z[1]), xlab = expression(z[1]))
hist(z[2, random_county, ], breaks = 100, freq = FALSE, main = expression(z[2]), xlab = expression(z[2]))
hist(z[3, random_county, ], breaks = 100, freq = FALSE, main = expression(z[3]), xlab = expression(z[3]))


##################
# Get posterior samples of age-adjusted rates
##################
aalami = array(dim = c(Ns, nsims))
std_pop = apply(n, 1, sum)  
w_a = std_pop / sum(std_pop)
for(i in 1:Ns){
  aalami[i,]= colSums(w_a * lami[, i, ])  
}

##################
# Calculate posterior medians
##################
beta_0a_med = apply(beta_0a[, (burnin+1):nsims], 1, median)
z_med = apply(z[, , (burnin+1):nsims], c(1,2), median)  
aa.med = apply(aalami[, (burnin+1):nsims], 1, median)

##################
##################
#THE BELOW CODE SHOULD BE LEFT AS-IS!
#IT ASSUMES YOU NAMED
#THE POSTERIOR MEDIANS OF THE AGE-ADJUSTED RATES
#"aamed" USING THE CODE ABOVE,
#AND WILL CREATE A MAP "PAmap.png"
#THAT WILL BE SAVED TO YOUR CURRENT DIRECTORY
##################

install.packages(c("sp", "rgdal", "rgeos", "maps", "RColorBrewer")) # I added this to install packages that I needed

load('penn.rdata')
install.packages(c('maps','RColorBrewer'))
library(maps)
library(RColorBrewer)
ncols=7
cols=brewer.pal(ncols,'RdYlBu')[ncols:1]
tcuts=quantile(aa.med*100000,1:(ncols-1)/ncols)
tcolb=array(rep(aa.med*100000,each=ncols-1) > tcuts,
            dim=c(ncols-1,Ns))
tcol =apply(tcolb,2,sum)+1

png('PAmap.png',height=520,width=1000)
par(mar=c(0,0,0,10),cex=1)
plot(penn,col=cols[tcol],border='lightgray',lwd=.5)
legend('right',inset=c(-.15,0),xpd=TRUE,
       legend=c(paste(
         c('Below',round(tcuts[-(ncols-1)],0),'Over'),
         c(' ',rep( ' - ',ncols-2),' '),
         c(round(tcuts,0),round(tcuts[ncols-1],0)),sep='')),
       fill=cols,title='Deaths per 100,000',bty='n',cex=1.5,
       border='lightgray')
dev.off()
