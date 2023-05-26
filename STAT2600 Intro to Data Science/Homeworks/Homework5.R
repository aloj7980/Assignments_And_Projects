library(dplyr)
library(stringr)
library(readr)
gehDf = read_csv("GreenEggsAndHam_IsSam.csv", show_col_types = FALSE)
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
ngrams = c("i", "you", "them", "not")
ngramDesMat = one_hot_encode_ngrams(lines = gehDf$Line, ngrams = ngrams)
ngramDf = cbind.data.frame(IsSam = gehDf$IsSam, ngramDesMat)
ngramLogit = glm(formula = IsSam ~ ., data = ngramDf, family = "binomial")
summary(ngramLogit)
log_loss <- function(yTrue, yPred){
  -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))
}
c(
  LogLoss = log_loss(yTrue = gehDf$IsSam, yPred = ngramLogit$fitted.values),
  AIC = ngramLogit$aic
)
featureSummary = cbind.data.frame(
  Feature = c("Intercept", colnames(ngramDf)[-1]),
  Coefficient = ngramLogit$coefficients,
  PValue = summary(ngramLogit)$coefficients[, 4]
)
remove_dependent_columns <- function(matrix){
  qrFact = qr(matrix)
  return(
    matrix[, qrFact$pivot[1:qrFact$rank]]
  )
}
designMatrix = ngramDf[2:5] %>% remove_dependent_columns()
write_csv(featureSummary, "FeatureSummary.csv")
write_csv(designMatrix, "DesignMatrix.csv")