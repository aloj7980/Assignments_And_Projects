#### Lab 10 ####

library(dplyr)
library(readr)

root_mean_square_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(sqrt(mean((yTrue - yPred)^2)))
}

mean_abs_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(mean(abs(yTrue - yPred)))
}

mean_abs_rel_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(mean(abs(yTrue - yPred)/yTrue))
}

compute_metrics <- function(yTrue_t, yPred_t, yTrue_e, yPred_e){
    c(
        root_mean_square_error(yTrue_t, yPred_t),
        root_mean_square_error(yTrue_e, yPred_e),
        mean_abs_error(yTrue_t, yPred_t),
        mean_abs_error(yTrue_e, yPred_e),
        mean_abs_rel_error(yTrue_t, yPred_t),
        mean_abs_rel_error(yTrue_e, yPred_e)
    )
}

#my.directory <- "C:/Users/zachs/Downloads"
#setwd(my.directory)

houseDf_t = read_csv("house_train.csv")
houseDf_e = read_csv("house_eval.csv")

# Define Column Names

featColNames = c("LotArea", "YearsOld", "TotRmsAbvGrd", "GrLivArea")
metricNames = c("RMSE_t", "RMSE_e", "MAE_t", "MAE_e", "MARE_t", "MARE_e")

# These are the oberved values of the response (SalePrice)

yVals_t = houseDf_t$SalePrice
yVals_e = houseDf_e$SalePrice

# Construct empty matrix

modelSummary = matrix(ncol = 11, nrow = 15)
colnames(modelSummary) = c("Intercept", featColNames, metricNames)

modelNames = NULL
ijModel = 1

# Nested for loop builds out matrix

for(iNFeats in 1:4){
    iFeatCombs = combn(1:4, iNFeats)
    
    for(jFeatComb in 1:ncol(iFeatCombs)){
        ijFeatComb = iFeatCombs[, jFeatComb]
        modelNames = c(modelNames, paste0("Feats_", 
                             paste(ijFeatComb, collapse = "")))
        ijFeats = featColNames[ijFeatComb]
        ijDesMat_t = cbind(1, houseDf_t %>% 
                               select(one_of(ijFeats)) %>% as.matrix())
        ijCoeffs = solve(t(ijDesMat_t) %*% ijDesMat_t, t(ijDesMat_t) %*% yVals_t)
        ijPreds_t = ijDesMat_t %*% ijCoeffs %>% as.vector()
        
        ijDesMat_e = cbind(1, houseDf_e %>% 
                               select(one_of(ijFeats)) %>% as.matrix())
        ijPreds_e = ijDesMat_e %*% ijCoeffs %>% as.vector()
        
        modelSummary[ijModel, c(0, ijFeatComb) + 1] = ijCoeffs
        modelSummary[ijModel, 6:11] = compute_metrics(
            yVals_t, ijPreds_t, yVals_e, ijPreds_e)

        ijModel = ijModel + 1
    }
}

# Sort final data frame by mean absolute relative error on evaluation set

cbind.data.frame(
        Model = modelNames,
        modelSummary
    ) %>% 
    as_tibble() %>% 
    arrange(
        MARE_e
    ) %>%
    write_csv(
        "Lab10.csv"
    )
    
