library(dplyr)
library(readr)


#### Transformation of Features/Response ####
nVals = 345

## samples from a contiuous uniform (0,1)
runif(1)

## or samples 10 uniform (0,1)
runif(10)
hist(runif(10^4))

## also rnorm samples from a normal(mean = 0, sd =1)
rnorm(1)

rnorm(1000) %>% mean()
rnorm(1000) %>% sd()

hist(rnorm(10^5), freq = FALSE, breaks = 50)

### generating synthetic data
xVals = runif(nVals, 5, 45)
yVals = 23 + 0.43 * xVals + 0.12 * xVals^2 + rnorm(nVals, sd = 20)
plot(xVals, yVals)


dataMatrix = cbind.data.frame(
    xVals  = xVals,
    xVals_sq = xVals^2,
    yVals = yVals
)


lineLm = lm(yVals ~ xVals, data = dataMatrix)
quadLm = lm(yVals ~ ., data = dataMatrix)
class(quadLm)

abline(lineLm$coefficients[1], lineLm$coefficients[2], col = "red")
plot(lineLm$fitted.values, lineLm$residuals)
abline(0, 0, col = "blue")

### plotting curve that isn't a line
plot(xVals, yVals)
xGrid = seq(from = min(xVals), to = max(xVals), length.out = 10^3)
yVals_quadModel = (cbind(1, xGrid, xGrid^2) %*% quadLm$coefficients) %>% 
    as.vector()
# 
# yVals_fromPredict = predict(
#     quadLm, 
#     cbind.data.frame(
#         xVals = xGrid, 
#         xVals_sq = xGrid^2)
# )
# 
# ### a mystery... for you to solve
# yVals_quadModel == yVals_fromPredict
# yVals_quadModel - yVals_fromPredict

lines(xGrid, yVals_quadModel, col = "blue", lwd = 4)
abline(lineLm$coefficients[1], lineLm$coefficients[2], col = "red")


plot(quadLm$fitted.values, quadLm$residuals)
abline(0, 0, col = "blue")


#### Determining Model Quality ####
## Bootstrapping 
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

r_squared <- function(yTrue, yPred){
    ssr = sum((yTrue - yPred)^2)
    yBar = mean(yTrue)
    tss = sum((yTrue - yBar)^2)
    1 - (ssr/tss)
}

houseDf = read_csv("Labs/Lab11_20221101/house_df_lab11.csv")

## feature with all feature on 
lm_all = lm(SalePrice ~ ., data = houseDf %>% select(-Id))

summary(lm_all)
r_squared(houseDf$SalePrice, lm_all$fitted.values)

houseSplitList = train_test_split(houseDf)

## splitting datasets
houseDf_train = houseSplitList$train
houseDf_test = houseSplitList$test

dim(houseDf)
dim(houseDf_train)
dim(houseDf_test)

## building model with just the training set
lm_train = lm(SalePrice ~ ., houseDf_train %>% select(-Id))

## predicting on the test set
testPreds = predict(lm_train, houseDf_test)

r_squared(houseDf_test$SalePrice, testPreds)
r_squared(houseDf_train$SalePrice, lm_train$fitted.values)
