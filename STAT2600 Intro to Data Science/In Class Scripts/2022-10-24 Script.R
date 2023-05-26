#### All Linear Models ####

## Q: Given 10 features, how many linear models can I create? 
## A: 2^10

## Q: How many models would have exactly 4 features
## A: 10 choose 4

choose(10, 4)
factorial(10)/(factorial(6) * factorial(4))

## `choose` tells you how many;
## meanwhile the function `combn` will provide all k-tuples from n 
library(readr)
library(dplyr)
library(tidyverse)

energyDf = read_csv("Data/energy_dataset_cleaned.csv")
energyDf
energyDf %>% colnames() %>% dput()
energyDf %>% dim()

## Q: how would I build all models with three features

## there are 5C3 = 10 such models 
choose(5, 3)

## we can select all 10 triples with the `combn` fuction
energyTypes = c("Fossil", "Hydro", "Wind", "Solar", "Nuclear")
combn(energyTypes, 3)
combn(length(energyTypes), 3)



#### Hypothesis Testing ####

## Q: is a particular coin fair
### H0 (null hypothesis): the coin is fair (i.e. P(Heads) = 1/2)
### H1 (alt hypothesis): the coin is not fair (i.e. P(Heads) != 1/2)

## test statistic: summary of the data collected test hypothesis. for the coin we might flip it 10 times and count the number of heads
### suppose for a particular test we get 2 heads 

### p-value: probability that "as extreme" of a value as the test statistic occurs given the null hypothesis is true. 

## the probability of getting exactly 2 heads (i.e. P(H = 2))
choose(10, 2)/2^10

## 1 or 0 heads are more extreme values so p value is P(H <= 2)
(choose(10, 2) + choose(10, 1)+ choose(10, 0))/2^10

## can also use `pbinom`
pbinom(2, 10, 1/2)

## alpha/false positive rate; the proportion of false positive rejections of the null hypothesis; conventionally 0.05

### if the p-value is less than alpha then we reject the null hypothesis and say "there is statistically significant evidence to support the alternative hypothesis. 


#### Multiple Hypothesis Tests ####
## If we perform a hypothesis test many times then we will almost surely turn up "significant results

## simulating lots of hypothesis tests. 
nFlips = 10
nSims = 10^4

coinFlipsMatrix = matrix(
    sample(0:1, size = nFlips*nSims, replace = TRUE),
    nrow = nSims,
    ncol = nFlips)

## the number of tests with the "rare" event of 0, 1, or 2 heads. 
sum(rowSums(coinFlipsMatrix) <= 2)








