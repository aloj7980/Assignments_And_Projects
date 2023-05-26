### Lab 9 ###

  # required libraries

require(dplyr)
require(readr)
require(stringr)

  # provided functions
  # make sure you are using '^' for the exponent in line 14

root_mean_square_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(sqrt(mean((yTrue - yPred)^2)))
}

mean_abs_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(mean(abs(yTrue - yPred)))
}

mean_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(mean(yTrue - yPred))
}

  #  Set your working directory

#myDirectory <- "C:/Users/zachs/Downloads"
#setwd(myDirectory) 

  #  Read in training dataset and evalation dataset

houseDf_t = read_csv("house_train.csv") %>% 
    mutate(
        LotArea_clipped = pmin(LotArea, 25000),
        YearsOld = YrSold - YearRemodAdd
    )

houseDf_e = read_csv("house_eval.csv") %>% 
    mutate(
        LotArea_clipped = pmin(LotArea, 25000),
        YearsOld = YrSold - YearRemodAdd
    )

  #  List of feature names

featColNames = c("LotArea_clipped", "YearsOld", "TotRmsAbvGrd", "GrLivArea")

  #  Observed value of the response (SalePrice) for training and evaluation sets

yVals_t = houseDf_t$SalePrice
yVals_e = houseDf_e$SalePrice

  #  Initiate modelSummary as null object

modelSummary = NULL

  #  For loop iterates through the 4 feature names

for(iFeature in featColNames){
  
  # pull observed values for each feature from both datasets
  
    iXVals_t = pull(houseDf_t, iFeature)
    iXVals_e = pull(houseDf_e, iFeature)
    
  # construct design matrix for each feature for training set
  # (column of 1s, column of observed values)  
    
    iDesMat_t = cbind(1, iXVals_t)
    
  # solve for the regression coefficients (intercept and slope)  
  # make sure you are using '%*%' for matrix multiplication (not '%>%')  
    
    iCoeffs = solve(t(iDesMat_t) %*% iDesMat_t, t(iDesMat_t) %*% yVals_t)
  
  # visualize response vs feature relationship with regression line for each feature
    
    plot(iXVals_t, yVals_t, main = paste(iFeature, "LM"))
    
  # add regression line
    
    abline(iCoeffs[1], iCoeffs[2], col = "blue")
    
  # compute predictions for training set
    
    iPreds_t = iDesMat_t %*% iCoeffs %>% as.vector()
    
  # construct design matrix for each feature in evaluation set
    
    iDesMat_e = cbind(1, iXVals_e)
    
  # compute predictions for evaluation set
    
    iPreds_e = iDesMat_e %*% iCoeffs %>% as.vector()
    
  # construct model Summary  
    
    modelSummary = rbind(
        modelSummary,
        c(as.vector(iCoeffs), 
          root_mean_square_error(yVals_t, iPreds_t),
          root_mean_square_error(yVals_e, iPreds_e),
          mean_abs_error(yVals_t, iPreds_t),
          mean_abs_error(yVals_e, iPreds_e),
          mean_error(yVals_t, iPreds_t),
          mean_error(yVals_e, iPreds_e)
        )
    )
}

  # set column names for model summary
colnames(modelSummary) = c(
    "Intercept", "Slope", 
    "RMSE_t", "RMSE_e", 
    "MAE_t", "MAE_e", 
    "ME_t", "ME_e")

  # add 'Feature' column and write to csv

cbind.data.frame(
    Feature = featColNames,
    modelSummary
) %>% write_csv(
    "lab9.csv"
)

 