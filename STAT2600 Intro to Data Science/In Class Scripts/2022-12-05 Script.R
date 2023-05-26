#### Project Guidelines ####
## see Canvas assignment and `ProjectRequirements.pdf`. 

#### HW 5 ####
library(readr)
library(dplyr)
library(stringr)
library(tidytext)

## dataset
gehDf <- read_csv("Homeworks/HW5/GreenEggsAndHam_IsSam.csv")
gehDf

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

## Words i.e. "Unigrams"
gehWords = gehDf$Line %>% str_split(" ") %>% 
    unlist() %>% unique() %>% sort()

## 2-grams i.e. "Bigrams"
gehBigrams = gehDf %>% 
    unnest_tokens(output = Bigrams, input = Line, token = "ngrams", n = 2) %>% 
    pull(Bigrams) %>% unique() %>% sort()

## 3-grams i.e. "Trigrams"
gehTrigrams = gehDf %>% 
    unnest_tokens(output = Trigrams, input = Line, token = "ngrams", n = 3) %>% 
    pull(Trigrams) %>% unique() %>% sort()

nGrams_oheMatrix = cbind.data.frame(
    one_hot_encode_ngrams(ngrams = gehWords, lines = gehDf$Line),
    one_hot_encode_ngrams(ngrams = gehBigrams, lines = gehDf$Line),
    one_hot_encode_ngrams(ngrams = gehTrigrams, lines = gehDf$Line)
)

ngramDf = cbind.data.frame(IsSam = gehDf$IsSam, nGrams_oheMatrix) %>% as_tibble()
ngramLogit = glm(
    formula = IsSam ~ `i` + `are` + `could you` + `green eggs and`, 
    data = ngramDf, 
    family = "binomial")
summary(ngramLogit)

predict(ngramLogit, ngramDf)
logit <- function(x){1/(1 + exp(-x))}
log_loss <- function(yTrue, yPred){
    -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))}

logit(predict(ngramLogit, ngramDf))
log_loss(gehDf$IsSam, logit(predict(ngramLogit, ngramDf)))


#### Singular Design Matrix ####
remove_dependent_columns <- function(matrix){
    qrFact = qr(matrix)
    return(
        matrix[, qrFact$pivot[1:qrFact$rank]]
    )
}

geahNgramMatrix = one_hot_encode_ngrams(
    ngrams = c("green", "eggs", "and", "ham"), lines = gehDf$Line)

head(geahNgramMatrix, 10)
tail(geahNgramMatrix, 10)

c(
    GreenEqEggs = all(geahNgramMatrix[, "green"] == geahNgramMatrix[, "eggs"]),
    EggsEqHam = all(geahNgramMatrix[, "eggs"] == geahNgramMatrix[, "ham"]),
    HamEqGreen = all(geahNgramMatrix[, "ham"] == geahNgramMatrix[, "green"])
)

geahNgramMatrix_trimmed = geahNgramMatrix %>% remove_dependent_columns()
geahNgramMatrix_trimmed %>% head(10)
geahNgramMatrix_trimmed %>% tail(10)

## rank of a matrix indicates how many linearly independent columns a matrix contains
### can be calculated with `Matrix::rankMatrix()` function
library(Matrix)
rankMatrix(geahNgramMatrix)
rankMatrix(geahNgramMatrix)[1]
rankMatrix(geahNgramMatrix_trimmed)[1]

### `nGrams_oheMatrix` constructed above; contains all ngrams for n<=3 
rankMatrix(nGrams_oheMatrix)[1]
nGrams_oheMatrix %>% dim()
nGrams_oheMatrix_trimmed = remove_dependent_columns(nGrams_oheMatrix)
dim(nGrams_oheMatrix_trimmed)
rankMatrix(nGrams_oheMatrix_trimmed)[1]
