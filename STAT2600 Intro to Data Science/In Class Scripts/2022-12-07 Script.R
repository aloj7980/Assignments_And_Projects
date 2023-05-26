#### Parallel Computing ####
library(readr)
library(abind)
source("Utils/train_test_split.R")
source("Utils/register_cores.R")

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

lbind <- function(x, y){
    abind(x, y, along = 3)
}

#### Serial #### 
## sometimes `for` loops (or iterative process) take needlessly too long
### needlessly in that they don't need to be executed consecutively 
### in these instances you might consider parallelizing them
houseDf = read_csv("Labs/Lab11_20221101/house_df_lab11.csv")

propTrain = (5:9)/10
nProps = length(propTrain)
nSims = 10^3


summaryArray = array(
    dim = c(3, nProps, nSims),
    dimnames = list(c("RMSE", "MAE", "R2"), propTrain))

{
    startTime_s = Sys.time()
    
    for(iPropInd in 1:nProps){
        iProp = propTrain[iPropInd]
        for(jSim in 1:nSims){
            ## split the data
            ijSplitList = train_test_split(houseDf, propTrain = iProp)
            ijTrain = ijSplitList$train
            ijTest = ijSplitList$test
            
            ## train the model
            iiLm = lm(SalePrice~., data = ijTrain)
            ijTestPreds = predict(iiLm, ijTest)
            
            ## calculate/store statistics
            summaryArray[, iPropInd, jSim] = c(
                root_mean_square_error(ijTest$SalePrice, ijTestPreds),
                mean_abs_rel_error(ijTest$SalePrice, ijTestPreds),
                r_squared(ijTest$SalePrice, ijTestPreds)
            )
        }
    }
    endTime_s = Sys.time()
    endTime_s - startTime_s
}

summaryArray %>% dim


## necessary packages
library(foreach)
library(iterators)
library(parallel)
library(doParallel)

## registering the 
nCores = detectCores() - 1
myCluster <- makeCluster(spec = nCores, type = "PSOCK")
registerDoParallel(cl = myCluster)
# stopCluster(cl = myCluster)


{
    startTime_p = Sys.time()
    summaryList <- foreach(
        iPropInd = 1:nProps,
        .combine = lbind
    ) %dopar% {
        iProp = propTrain[iPropInd]
        iSummaryMatrix = matrix(nrow = 3, ncol = nSims)
        for(jSim in 1:nSims){
            ## split the data
            ijSplitList = train_test_split(houseDf, propTrain = iProp)
            ijTrain = ijSplitList$train
            ijTest = ijSplitList$test
            
            ## train the model 
            iiLm = lm(SalePrice~., data = ijTrain)
            ijTestPreds = predict(iiLm, ijTest)
            
            ## calculate/store statistics
            iSummaryMatrix[, jSim] = c(
                root_mean_square_error(ijTest$SalePrice, ijTestPreds),
                mean_abs_rel_error(ijTest$SalePrice, ijTestPreds),
                r_squared(ijTest$SalePrice, ijTestPreds)
            )
        }
        iSummaryMatrix
    }
    
    endTime_p = Sys.time()
    endTime_p - startTime_p
}

summaryList %>% dim


#### GG Plot/Visualization ####
## ggplot 
library(ggplot2)
library(dplyr)

## big difference between ggplot and base graphics is that ggplot works with dataframes while base graphics work with vectors
midwest = ggplot2::midwest
midwest %>% 
    filter(
        poptotal < 10^6
    ) %>% 
    ggplot(aes(x = area, y = poptotal)) +
    geom_point(aes(col = state)) +
    geom_smooth(method = "lm", col = "red") + 
    labs(title = "Area vs. Population", x = "Area", y = "Population")


## plotly
### my favorite package for plotting 3d plots

library(plotly)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, color = ~am, z = ~mpg, size = 1)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'wt'),
                                   yaxis = list(title = 'hp'),
                                   zaxis = list(title = 'mpg')))

fig






