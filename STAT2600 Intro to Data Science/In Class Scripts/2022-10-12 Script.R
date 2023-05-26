#### Homework/Lab Questions ####
library(readr)
library(dplyr)

collegesDf = read_csv("Homeworks/HW3/colleges.csv") 
countiesDf = read_csv("Homeworks/HW3/counties.csv")
maskUseDf = read_csv("Homeworks/HW3/mask_use_by_county.csv")

countiesDf
source("Labs/Lab8_20221011/one_hot_encode.R")
houseDf = read_csv("Labs/Lab8_20221011/house_price_train_2.csv") 

data.frame(
    Col1 = logical(10),
    Col2 = numeric(10)
)

#### Modeling ####

## two types of models: predictive and data discovery models
### AKA supervised and unsupervised models/learning

## exploration data versus confirmation data
### train, query, test 

#### Overfitting: DONT DO THIS ####

library(xgboost)
library(readr)
library(dplyr)

houseDf = read_csv(paste0(
    "Data/house-prices-advanced-", 
    "regression-techniques/house_price_train.csv"))

xgbFeatures = c(
    "LotArea", "FullBath", "YrSold", "GrLivArea",
    "BedroomAbvGr", "KitchenAbvGr", "OverallQual")

xgbModel = xgboost(
    data = houseDf %>% 
        select(one_of(xgbFeatures)) %>% as.matrix(),
    params = list(
        eta = .5
    ),
    label = houseDf$SalePrice,
    print_every_n = 100,
    nrounds = 10000
)

xgbPreds = predict(xgbModel, houseDf %>% 
            select(one_of(xgbFeatures)) %>% as.matrix())
length(predictions)
dim(houseDf)

root_mean_square_error <- function(trueVals, predVals){
    sqrt(mean((trueVals - predVals)^2))
}


#### RMSE: Root Mean Square Error
root_mean_square_error(houseDf$SalePrice, xgbPreds)


### to avoid overfitting use eval datasets
### need to use xgb.train and xgb.DMatrix

houseDmat = xgb.DMatrix(
    data = houseDf %>% 
        select(one_of(xgbFeatures)) %>% 
        as.matrix(),
    label = houseDf$SalePrice)

xgbModel = xgb.train(
    data = houseDmat,
    params = list(eta = .5),
    print_every_n = 100,
    nrounds = 10000,
    watchlist = list(train = houseDmat),
    early_stopping_rounds = 10
)


#### Splitting Datatsets ####
## train, test/validation/evaluation/query, holdout splits
dataset = houseDf

train_test_split <- function(dataset, prop_train = .8){
    nrowTrain = nrow(dataset) * prop_train
    allInds = 1:nrow(dataset)
    trainingInds = sample(allInds, size = nrowTrain)
    testInds = (allInds)[!(allInds %in% trainingInds)]
    return(
        list(
            train = dataset[trainingInds, ],
            test = dataset[testInds, ]
        )
    )
}

trainTestList = train_test_split(houseDf)


### take two: eval set with test dataset

trainDmat= xgb.DMatrix(
    data = trainTestList$train %>% 
        select(one_of(xgbFeatures)) %>% 
        as.matrix(),
    label = trainTestList$train$SalePrice)

testDmat= xgb.DMatrix(
    data = trainTestList$test %>% 
        select(one_of(xgbFeatures)) %>% 
        as.matrix(),
    label = trainTestList$test$SalePrice)

xgbModel = xgb.train(
    data = trainDmat,
    params = list(eta = .1),
    print_every_n = 10,
    nrounds = 5000,
    watchlist = list(train = trainDmat, test = testDmat),
    early_stopping_rounds = 100
)

