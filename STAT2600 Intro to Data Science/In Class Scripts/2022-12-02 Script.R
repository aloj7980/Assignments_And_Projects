#### HW 5 ####
library(readr)
library(dplyr)
library(stringr)
library(tidytext)

## dataset
gehDf <- read_csv("Homeworks/HW5/GreenEggsAndHam_IsSam.csv")
gehDf
gehDf %>% dim

## words
gehWords = gehDf$Line %>% 
    str_split(" ") %>% 
    unlist() %>% 
    unique() %>% 
    sort()


## n-grams
gehBigrams = gehDf %>% 
    unnest_tokens(output = Bigrams, input = Line, token = "ngrams", n = 2) %>% 
    pull(Bigrams) %>% 
    unique() %>% 
    sort()

gehTrigrams = gehDf %>% 
    unnest_tokens(output = Trigrams, input = Line, token = "ngrams", n = 3) %>% 
    pull(Trigrams) %>% 
    unique() %>% 
    sort()


#### One-Hot-Encoding N-Grams ####

get_next_n_els <- function(els, start, n){
    els[start:(start + n - 1)]
}

ngram_detect <- function(line, ngram){
    nNgram = length(unlist(str_split(ngram, " ")))
    lineWords = unlist(str_split(line, " "))
    nLineWords = length(lineWords)
    if(nNgram > nLineWords){
        return(FALSE)
    }
    if(nNgram == nLineWords){
        return(ngram == line)
    }
    if(nNgram == 1){
        lineNgrams = lineWords
    } else{
        lineNgrams = apply(
            X = sapply(1:(nLineWords-nNgram+1), 
                       get_next_n_els, 
                       els = lineWords, n = nNgram), 
            MARGIN = 2, 
            FUN = paste, collapse = " ")
    }
    
    any(ngram == lineNgrams)
}

one_hot_encode_ngrams_line <- function(line, ngrams){
    return(setNames(
        as.numeric(sapply(ngrams, ngram_detect, line = line)),
        ngrams
    ))
}

one_hot_encode_ngrams <- function(lines, ngrams){ 
    return(
        t(sapply(lines, one_hot_encode_ngrams_line, ngrams = ngrams)))
}


ngrams = c("eat", "i", "good", "me", "you", "could not", "you may",
           "do not like", "sam i am", "green eggs and ham")

oheNgramsDesMat = one_hot_encode_ngrams(lines = gehDf$Line, ngrams = ngrams)
colnames(oheNgramsDesMat)
rownames(oheNgramsDesMat)
oheNgramsDesMat

ngramDf = cbind.data.frame(IsSam = gehDf$IsSam, oheNgramsDesMat)
ngramLogit = glm(formula = IsSam ~ ., data = ngramDf, family = "binomial")
summary(ngramLogit)


predict(ngramLogit, ngramDf)
logit <- function(x){1/(1 + exp(-x))}

logit(predict(ngramLogit, ngramDf))


#### Singular Design Matrix ####
remove_dependent_columns <- function(matrix){
    qrFact = qr(matrix)
    return(
        matrix[, qrFact$pivot[1:qrFact$rank]]
    )
}

gehNgramMatrix = one_hot_encode_ngrams(
    ngrams = c("green", "eggs", "and", "ham"), lines = gehDf$Line)

head(gehNgramMatrix, 10)

c(
    GreenEqEggs = all(gehNgramMatrix[, "green"] == gehNgramMatrix[, "eggs"]),
    EggsEqHam = all(gehNgramMatrix[, "eggs"] == gehNgramMatrix[, "ham"]),
    HamEqGreen = all(gehNgramMatrix[, "ham"] == gehNgramMatrix[, "green"])
)

gehNgramMatrix %>% remove_dependent_columns() %>% head(10)



#### Parallel Computing ####

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


