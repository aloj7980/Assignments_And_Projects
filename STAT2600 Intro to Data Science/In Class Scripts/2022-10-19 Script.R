library(dplyr)
library(readr)

#### Lab 9 ####
houseDf_t = read_csv("Labs/Lab9_20221018/house_train.csv") %>% 
    mutate(
        LotArea_clipped = pmin(LotArea, 25000),
        YearsOld = YrSold - YearRemodAdd
    )

houseDf_e = read_csv("Labs/Lab9_20221018/house_eval.csv") %>% 
    mutate(
        LotArea_clipped = pmin(LotArea, 25000),
        YearsOld = YrSold - YearRemodAdd
    )

featColNames = c("LotArea_clipped", "YearsOld", "TotRmsAbvGrd", "GrLivArea")

### Q: can I plot dates/values ?
### A: yes maybe use the time series object (`ts`) built into R
#### Otherwise just use `plot(dateVec, yValues)`
ts(sample(10))

plot(ts(sample(10)))



#### Multi-Linear Regression ####
houseDf = read_csv(paste0(
        "Data/house-prices-advanced-", 
        "regression-techniques/house_price_train.csv")
    ) %>% 
    mutate(
        LotArea_clipped = pmin(LotArea, 25000),
        YearsOld = YrSold - YearRemodAdd
    )




### modeling with square footage 
plot(houseDf_t$GrLivArea, houseDf_t$SalePrice, cex = .4)

designMat = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea
)

coeffVect = solve(t(designMat) %*% designMat, t(designMat) %*% houseDf$SalePrice)

abline(coeffVect[1], coeffVect[2], col = "blue")


### modeling with square footage and ....

featColNames = c("LotArea_clipped", "YearsOld", "TotRmsAbvGrd", "GrLivArea")

## square footage and age
designMat_2 = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea,
    Age = houseDf$YearsOld
)

coeffVect_2 = solve(t(designMat_2) %*% designMat_2, t(designMat_2) %*% houseDf$SalePrice)

coeffVect_2


### square footage and lot area
designMat_2.1 = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea,
    LotArea = houseDf$LotArea_clipped
)

coeffVect_2.1 = solve(t(designMat_2.1) %*% designMat_2.1, t(designMat_2.1) %*% houseDf$SalePrice)

coeffVect_2.1


root_mean_squared_error <- function(
        yTrue = houseDf$SalePrice, yPred
){
    sqrt(mean((yTrue - yPred)^2))
}

### considering rmse for models
root_mean_squared_error(yPred = designMat %*% coeffVect)
root_mean_squared_error(yPred = designMat_2 %*% coeffVect_2)
root_mean_squared_error(yPred = designMat_2.1 %*% coeffVect_2.1)

### using all four covariates 
designMat_4 = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea,
    Age = houseDf$YearsOld,
    LotArea = houseDf$LotArea_clipped,
    NRooms = houseDf$TotRmsAbvGrd
)

coeffVect_4 = solve(t(designMat_4) %*% designMat_4, t(designMat_4) %*% houseDf$SalePrice)

coeffVect_4

### taking out nRooms
designMat_3 = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea,
    Age = houseDf$YearsOld,
    LotArea = houseDf$LotArea_clipped
)

coeffVect_3 = solve(t(designMat_3) %*% designMat_3, t(designMat_3) %*% houseDf$SalePrice)

coeffVect_3

### 
designMat_3.1 = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea,
    Age = houseDf$YearsOld,
    NRooms = houseDf$TotRmsAbvGrd
)

coeffVect_3.1 = solve(t(designMat_3.1) %*% designMat_3.1, t(designMat_3.1) %*% houseDf$SalePrice)

coeffVect_3.1

### 

root_mean_squared_error(yPred = designMat_2 %*% coeffVect_2)
root_mean_squared_error(yPred = designMat_4 %*% coeffVect_4)

#### Using `lm` Function ####

multLmDf = houseDf %>% 
    select(
        one_of(featColNames),
        SalePrice
    )

## using `lm`; first argument is "formula"
### left hand side of formula is what you are predicting
### right hand side is what covariates you use to predict with
#### "." means use all covariates
multLm = lm(SalePrice ~ ., data = multLmDf)

## the coefficient match!!
coeffVect_4


## we can call our friend `summary` on our `lm` object
summary(multLm) 


### model object have a few nice stored variables

multLm$coefficients
multLm$residuals

sqrt(mean((multLm$residuals)^2))

## these are the predictions
multLm$fitted.values


### can also predict with these model object using `predict`

predict(multLm, houseDf %>% select(one_of(featColNames)))

