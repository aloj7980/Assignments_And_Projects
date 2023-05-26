#### HW4 ####

rownames(mtcars)
colnames(mtcars)

carsLm = lm(mpg ~ cyl + hp, data = mtcars)
summary(carsLm)

## plotting cyl vs residuals
plot(mtcars$cyl, carsLm$residuals)
abline(0, 0, col = "blue")

## plotting hp vs residuals
plot(mtcars$hp, carsLm$residuals)
abline(0, 0, col = "blue")

## plotting fitted values vs residuals
plot(carsLm$fitted.values, carsLm$residuals)
abline(0, 0, col = "blue")

#### Non-linear Linear Models? ####
## AKA Transforming Models 

## height (cm) and weight (kg)

library(readr)
htWtDf = read_csv("Data/HeightWeight.csv")

plot(htWtDf$height, htWtDf$weight)
linModel = lm(weight ~ height, data = htWtDf)
class(linModel)
abline(linModel$coefficients[1], linModel$coefficients[2], col = "blue")

## residuals are difference between predictions and true values
plot(htWtDf$height, linModel$residuals)
abline(0,0, col = "blue")

## when you have multiple features included in a model, plot the fitted values against the residual first to inspect them
plot(linModel$fitted.values, linModel$residuals)
abline(0,0, col = "blue")

carsLm = lm(mpg ~ cyl + hp, data = mtcars)
summary(carsLm)


## ASIDE: an example with more than one feature
## plotting cyl vs residuals
plot(mtcars$cyl, carsLm$residuals)
abline(0, 0, col = "blue")

## plotting hp vs residuals
plot(mtcars$hp, carsLm$residuals)
abline(0, 0, col = "blue")

## plotting fitted values vs residuals (check this first)
plot(carsLm$fitted.values, carsLm$residuals)
abline(0, 0, col = "blue")


## the residuals tell us that this model isn't quite right, because they aren't white noise (i.e they display a distinct non-linear pattern)

### to fix this we can transform our ht variable and add it to the design matrix.
htWtDf = htWtDf %>% 
    mutate(
        heightSq = height^2
    )
linModel_wSq = lm(weight ~ height + heightSq, data = htWtDf)

summary(linModel_wSq)

plot(linModel_wSq$fitted.values, linModel_wSq$residuals)
abline(0, 0, col = "blue")






