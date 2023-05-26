#### Lab 11 ####

# Libraries

require(dplyr)
require(readr)

# format out scientific notation
options(scipen = 100)

# Functions

train_test_split <- function(dataset, propTrain = .8, seed = NULL){
  if(length(seed)){
    set.seed(seed)
  }
  
  nrowTrain = nrow(dataset) * propTrain
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

root_mean_square_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(sqrt(mean((yTrue - yPred)^2)))
}

mean_abs_rel_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(mean(abs(yTrue - yPred)/yTrue))
}

r_squared <- function(yTrue, yPred){
    ssr = sum((yTrue - yPred)^2)
    yBar = mean(yTrue)
    tss = sum((yTrue - yBar)^2)
    1 - (ssr/tss)
}

# Import Lab 11 dataset

my.directory <- "C:/Users/zachs/Downloads"
setwd(my.directory)
houseDf = read_csv("house_df_lab11.csv")

# Initialize for loops

propTrain = (5:9)/10
nSims = 10^3
summaryDf = NULL

# Nested for loops

for(iProp in propTrain){
  # initialize 3 X 1000 empty matrix
    iSummaryMatrix = matrix(nrow = 3, ncol = nSims)
    for(jSim in 1:nSims){
      # split data into train and test sets
        ijSplitList = train_test_split(houseDf, propTrain = iProp)
        ijTrain = ijSplitList$train
        ijTest = ijSplitList$test
      # fit model on train set  
        iiLm = lm(SalePrice~., data = ijTrain)
      # compute predictions on test set  
        ijTestPreds = predict(iiLm, ijTest)
      # populate 3 X 1000 matrix  
        iSummaryMatrix[, jSim] = c(
            root_mean_square_error(ijTest$SalePrice, ijTestPreds),
            mean_abs_rel_error(ijTest$SalePrice, ijTestPreds),
            r_squared(ijTest$SalePrice, ijTestPreds)
        )
    }
    # compute means over 1000 columns
    summaryDf = cbind(summaryDf, apply(iSummaryMatrix, 1, mean))
    
}

# define column names
colnames(summaryDf) = paste0("TrainPercent_", propTrain*100)

# add metric column (these are effectively row names)
summaryDf = cbind.data.frame(
    Metric = c("RMSE", "MARE", "R2"),
    summaryDf
)

# write final data frame to .csv
summaryDf %>% write_csv("lab11.csv")


