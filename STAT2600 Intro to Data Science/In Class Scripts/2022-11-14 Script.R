#### Population vs Samples ####
## The game of probability is going from population to sample
## The game of statistics is going froim sample to population



#### Central Limit Theorem (Statement) ####
## the central limit theorem states:
### the distribution of the sample mean is approximately normal
#### the mean of this normal will be the population mean 
#### the standard deviation (sd) will be the population sd divided by the sqrt of the number of samples

xVals = (-2500:2500)/1000
plot(xVals, dnorm(xVals, sd = 1), type = "l")
lines(xVals, dnorm(xVals, sd = sqrt(1/512)), type = "l")

normalSds = sqrt(c(2^(-7:0)))
sdColors = rev(rainbow(length(normalSds)))
plot(xVals, dnorm(xVals, sd = normalSds[1]), 
      ylab = "Normal PDF", xlab = "",
     type = "l", col = sdColors[1])

for(iSdInd in 2:length(normalSds)){
    lines(xVals, dnorm(xVals, sd = normalSds[iSdInd]), 
          col= sdColors[iSdInd])
}

legend("topright", legend = paste0("n = ", 2^(0:7)), lty = 1, col = rev(sdColors))


#### Hypothesis Testing of Means ####
### brainstorm: think of a circumstance where you might be interested in whether or not the population mean is positive


n = 10^6
set.seed(1235)
data = c(
    rnorm(n, mean = .15, sd = 4),
    rnorm(n/2, mean = 12, sd = 3),
    rnorm(n/2, mean = -12, sd = 3)
) %>% sample(size = 187654)

data %>% hist(breaks = 25, xlab = "", main = paste("Sample Hist: X-bar = ", round(mean(data), 4)))



## to consider the question of whether or not the mean of this population is positive consider the following hypothesis test

## H0: mu = 0
## H1: mu > 0

### Given H0, what is the likelihood that X-bar the value above? 

data %>% mean()
data %>% sd()
data %>% range()
data %>% range() %>% diff()
data %>% length()

## p-value: assume the null hypothesis is true, what is the likelihood of the mean being 0.08? 


pnorm(mean(data), mean = 0, sd = sd(data)/sqrt(length(data)), lower.tail = FALSE)
1- pnorm(mean(data), mean = 0, sd = sd(data)/sqrt(length(data)))


#### Central Limit Theorem (Example) #### 
## a really weird distribution:
### 1/4 of the time return return unif(-5.1,-4.9)
### 1/2 of the time return a triangle distribution 
### 1/4 of the time return a shifted exponential

rtri <- function(n){
    abs(runif(n) - runif(n))
}

hist(rtri(10^5), breaks = 40, freq = FALSE)
abline(2, -2, col = "red")

rweird <- function(n){
    whichDist = sample(1:3, size = n, replace = TRUE, prob = c(1, 2, 1))
    ifelse(whichDist == 1, runif(n, -5.1, -4.9), 
           ifelse(whichDist == 2, rtri(n), rexp(n, rate=2) + 4))
}

hist(rweird(10^4), breaks = 60)

mean_weird = "question mark??"

n = 10^3
manyMeans_weird = matrix(
    rweird(n * 10^4), nrow = n, ncol = 10^4
) %>% colMeans()

hist(manyMeans_weird, breaks = 60)
plot_normal_density(mean = mean_weird, sd = .1, xlim = c(-1, 1))
