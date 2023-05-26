#### Lab 11

source("Labs/Lab11_20221101/lab11.R")


#### Multivariate Modeling

library(dplyr)

nVals = 345
xVals_1 = runif(nVals, 5, 45)
xVals_2 = rnorm(nVals, sd = 5)
xVals_3 = runif(nVals, max = 10) - runif(nVals, max = 30)
xVals_4 = rnorm(nVals, mean = 3,  sd = 10)

hist(xVals_1)
hist(xVals_2)
hist(xVals_3)
hist(xVals_4)

## generating a process; in real life, we only observe the yVals not the formula that generates it. 
yVals_1 = 1.4 * xVals_1 + 2.3 * xVals_2 - 0.8 * xVals_3 + rnorm(nVals, sd = 10)


## start by looking at plots. 
plot(xVals_1, yVals_1)
plot(xVals_2, yVals_1)
plot(xVals_3, yVals_1)
plot(xVals_4, yVals_1)


lm(yVals_1 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4)
lm(yVals_1 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4) %>% summary()

## can use paste to make this easier with lots of covariates.
lm(paste0("yVals_1 ~ ", paste0("xVals_", 1:4, collapse = " + "))) %>% summary()

## x4 had a high pvalue what happens if we take it out
lm(yVals_1 ~ xVals_1 + xVals_2 + xVals_3) %>% summary()

## intercpet still isn't significant
lm(yVals_1 ~ xVals_1 + xVals_2 + xVals_3 + 0) %>% summary()



### another synthetic process
yVals_2 = 1.4 * xVals_1 + .4* xVals_2^2 - 0.4 * xVals_4 + 
    rnorm(nVals, sd = 10)


plot(xVals_1, yVals_2)
plot(xVals_2, yVals_2)
plot(xVals_3, yVals_2)
plot(xVals_4, yVals_2)
lm(yVals_2 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4) %>% summary()

# not obious about some; but x3 looks like we can take it out
lm(yVals_2 ~ xVals_1 + xVals_2 + xVals_4) %>% summary()


lm(yVals_2 ~ xVals_1 + xVals_4) %>% summary()
xVals_2_sq = xVals_2^2 

# not obious about some; but x3 looks like we can take it out
lm(yVals_2 ~ xVals_1 + xVals_2_sq + xVals_4) %>% summary()


with(mtcars, plot(cyl, mpg))
### another one
yVals_3 = xVals_1 + .4* xVals_3^2 - 0.4 * xVals_4 + 
    rnorm(nVals, sd = 10)


plot(xVals_1, yVals_3)
plot(xVals_2, yVals_3)
plot(xVals_3, yVals_3)
plot(xVals_4, yVals_3)
lm(yVals_3 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4) %>% summary()



### another one
yVals_4 = exp(0.1 * xVals_1) + 0.4 * xVals_4 + 
    rnorm(nVals, sd = 10)


plot(xVals_1, yVals_4)
plot(xVals_2, yVals_4)
plot(xVals_3, yVals_4)
plot(xVals_4, yVals_4)
lm(yVals_4 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4) %>% summary()




### another one
yVals_5 = 0.1 * xVals_1 +  0.1 * xVals_2 * xVals_3 + 0.4 * xVals_4 + 
    rnorm(nVals, sd = 10)


plot(xVals_1, yVals_5)
plot(xVals_2, yVals_5)
plot(xVals_3, yVals_5)
plot(xVals_4, yVals_5)
lm(yVals_5 ~ xVals_1 + xVals_2 + xVals_3 + xVals_4) %>% summary()


