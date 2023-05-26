#### Lab 14 ####

library(dplyr)
library(readr)

##  code to produce original training and testing sets

# source("Utils/train_test_split.R")
#
# sample_rows <- function(df, frac){
#     nRowsSample <- round(frac*nrow(df))
#     return(df[sample(nrow(df), size = nRowsSample), ])
# }
#
# appDfList = read_csv("Data/home-credit-default-risk/application_train.csv") %>% 
#     transmute(
#         Defaults = TARGET,
#         IsMale = as.numeric(CODE_GENDER == "M"),
#         Age = -round(DAYS_BIRTH/365.25),
#         OwnsCar = as.numeric(FLAG_OWN_CAR == "Y"),
#         OwnsResidence = as.numeric(FLAG_OWN_REALTY == "Y"),
#         AnualIncome = AMT_INCOME_TOTAL,
#         LoanAmount = AMT_CREDIT
#     ) %>% 
#     sample_rows(1/3) %>% 
#     train_test_split()
# 
# appDf_train = appDfList$train
# appDf_test = appDfList$test
# 
# appDf_train %>% write_csv("application_train.csv")
# appDf_test %>% write_csv("application_test.csv")

# log_loss function

log_loss <- function(yTrue, yPred){-mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))}

# Import data

my.directory <- "C:/Users/zachs/Downloads"
setwd(my.directory)
appDf_train = read_csv("application_train.csv")
appDf_test = read_csv("application_test.csv")

# list of model features

feats = colnames(appDf_train)[-1]
nFeats = length(feats)
nRows = choose(nFeats, 2)
modelNames = character(nRows)

# create dictionary of feature names 

featIndDict = setNames(1:nFeats, feats)

# all combinations (pairs) of features

featCombos = combn(nFeats, 2)

# set number of rows and columns for summary Matrix

summaryMatrix = matrix(nrow = nRows, ncol = 4 + nFeats)

# build out data frame using for loop

for(iRow in 1:ncol(featCombos)){
  # iterate through all possible combinations (pairs) of features
    iFeatCombs = feats[featCombos[, iRow]]
  # fit logistic regression glm to training set
    iLogitModel = glm(paste0("Defaults ~ ", paste(iFeatCombs, collapse = " + ")), 
                      data = appDf_train, family = "binomial")
  # make predictions on test set  
    iPreds_test = predict(iLogitModel, newdata = appDf_test, type="response")
  # build out model names vector  
    modelNames[iRow] = paste0("Feats_", paste(featCombos[, iRow], collapse = ""))
  # build out model summary matrix
    summaryMatrix[iRow, c(1, featCombos[, iRow] + 1, 8:10)] = c(
  # estimated coefficients for intercept and each predictor variable  
        iLogitModel$coefficients,
  # log_loss metric computed on training and testing sets      
        log_loss(appDf_train$Defaults, iLogitModel$fitted.values),
        log_loss(appDf_test$Defaults, iPreds_test),
  # number of statistically significant predictor variables
        sum(summary(iLogitModel)$coefficients[, 4] <= 0.05)
    )
}

# Define column names

colnames(summaryMatrix) = c(
    "Intercept",
    paste0("C_", feats),
    "LogLoss_train",
    "LogLoss_eval",
    "NSigFeats"
)

# Merge model name vector and model summary matrix

cbind.data.frame(
    ModelName = modelNames,
    summaryMatrix
) %>% arrange(
    LogLoss_eval
) %>% write_csv("LogitModelSummaries.csv")

# Produce final data frame

LogitModelSummaries = read_csv("LogitModelSummaries.csv")
