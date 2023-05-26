#### The Central Limit Theorem ####
## The distribution of the mean is approximately normal
library(dplyr)
n = 10^4
unifSamp = runif(n)
hist(runif(n), freq = FALSE)
lines(0:1, c(1, 1), col = "red")

singleMean = mean(runif(n))
singleMean

manyMeans = matrix(
    runif(n*10^4), nrow = n, ncol = 10^4
) %>% colMeans()

hist(manyMeans, breaks = 40, freq = FALSE)

## this looks bell curvy... and it is! that's what the central limit theorem says!!!

plot_normal_density <- function(mean, sd, xlim){
    xVals = seq(xlim[1], xlim[2], length.out = 10^3)
    yVals = dnorm(xVals, mean = mean, sd = sd)
    lines(xVals, yVals, col = "red")
}

## it makes sense that the mean is 1/2, but what is the sd
plot_normal_density(mean = .5, sd = sqrt(1/12)/sqrt(n), xlim = c(.49, .51))

### the standard deviation of the sample mean distribution is the original distributions standard deviation divided by the sqrt of n. 

sd(runif(10^6))
1/12




### another distributions: the exponential 
n = 10^4
expRate = 1/5
expSamp = rexp(n, rate = expRate)
hist(expSamp, breaks = 50, freq = FALSE)
xVals = seq(min(expSamp), max(expSamp), length.out = 10^3)
lines(xVals, dexp(xVals, rate = expRate), col = "red")

lines(c(5, 5), c(0, 1), col = "blue")

mean(rexp(n, rate = expRate))

manyMeans_exp = matrix(
    rexp(n * 10^4, rate = expRate), nrow = n, ncol = 10^4
) %>% colMeans()

hist(manyMeans_exp, breaks = 40, freq = FALSE)

plot_normal_density(mean = 1/expRate, sd = (1/expRate)/sqrt(n), xlim = c(4.8, 5.2))


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


### Hypothesis testing for a mean
## consider the hypothesis test 
### H0: mu = 0
### H1: mu > 0
library(readr)
load("Data/HardEarnedData")
hist(heData, main = "Real-Faux Political Leaning", breaks = 40)



