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





#### Process 4 ####
### another one with an exponential term
yVals_4 = exp(0.1 * xVals_1) + 0.4 * xVals_4 + 
    rnorm(nVals, sd = 10)

### step 1: look at the plots of each covariate against repsonse variable
plot(xVals_1, yVals_4)
plot(xVals_2, yVals_4)
plot(xVals_3, yVals_4)
plot(xVals_4, yVals_4)


### step 2: consider correlations
varMatrix = cbind(
    C1 = xVals_1,
    C2 = xVals_2,
    C3 = xVals_3,
    C4 = xVals_4,
    Y = yVals_4,
    Y_l = log(yVals_4 - min(yVals_4) + 1)
)

head(varMatrix)
cor(varMatrix)

### trying out the models
lm(yVals_4 ~ xVals_1 + xVals_3 + xVals_4) %>% summary()

### transform xVal_1

xVal1_log = log(xVals_1)
plot(xVal1_log, yVals_4)
xVal1_sqrt = sqrt(xVals_1)
plot(xVal1_sqrt, yVals_4)


lm(yVals_4 ~ xVal1_log + xVals_3 + xVals_4) %>% summary()

lm(yVals_4 ~ xVal1_sqrt + xVals_3 + xVals_4) %>% summary()


yVal4_log = log(yVals_4 - min(yVals_4) + 1)
plot(xVals_1, yVal4_log)


lm(yVal4_log ~ xVals_1 + xVals_3 + xVals_4) %>% summary()

lm(yVal4_log ~ xVals_1 + xVals_4) %>% summary()

finalLm = lm(Y_l ~ C1 + C4, data = as.data.frame(varMatrix))


#### Estimating Model Quality w/ Bootstrapping ####

root_mean_square_error <- function(yTrue, yPred){
    stopifnot(length(yTrue) == length(yPred))
    return(sqrt(mean((yTrue - yPred)^2)))
}

root_mean_square_error(varMatrix[, 6], finalLm$fitted.values)


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


designMatrix = cbind.data.frame(
    C1 = xVals_1,
    C4 = xVals_4,
    # Y = yVals_4,
    Y_l = log(yVals_4 - min(yVals_4) + 1)
)


splitList = train_test_split(designMatrix)
trainDf = splitList$train
testDf = splitList$test
dim(trainDf)

colnames(trainDf)

lmTrain = lm(Y_l ~ . , data = trainDf)
root_mean_square_error(testDf$Y_l, predict(lmTrain, testDf))

dim(trainDf)
dim(testDf)

nSims = 10^2
testRmse = numeric(nSims)



for(iSim in 1:nSims){
    
}


#### Central Limit Theorem ####
## the mean of a distribution is nearly/approximately a normal distribution







