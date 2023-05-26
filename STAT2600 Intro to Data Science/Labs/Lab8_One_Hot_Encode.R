library(readr)
library(dplyr)

### one hot encode
one_hot_encode_line <- function(allWords, lineWords){
  as.numeric(allWords %in% lineWords)
}

one_hot_encode_lines <- function(allWords, lines, prefix = NULL, dropFirst = FALSE){
  lineWordsList = lines %>% str_split(" ")
  oheMatrix = t(sapply(lineWordsList, 
                       one_hot_encode_line, 
                       allWords = allWords))
  if(!is.null(prefix)){
    allWords = paste0(prefix, allWords)
  }
  colnames(oheMatrix) = allWords
  if(dropFirst){
    return(oheMatrix[, -1])
  } else{
    return(oheMatrix)
  }
}



houseDf = read_csv("house_price_train_2.csv") %>% 
  mutate(
    Conditions = paste(Condition1, Condition2)
  )

allConds = c(houseDf$Condition1, houseDf$Condition2) %>% unique() %>% sort()

houseDf_ohe = cbind.data.frame(
  Id = houseDf$Id,
  one_hot_encode_lines(
    allWords = houseDf$Neighborhood %>% unique() %>% sort(),
    lines = houseDf$Neighborhood,
    prefix = "Nbhd_"
  ),
  one_hot_encode_lines(
    allWords = allConds,
    lines = houseDf$Conditions,
    prefix = "Cond_"
  ),
  one_hot_encode_lines(
    allWords = houseDf$HouseStyle %>% unique() %>% sort(),
    lines = houseDf$HouseStyle,
    prefix = "HS_"
  ),
  LogSalePrice = log(houseDf$SalePrice)
) 

houseDf_ohe %>% write_csv("lab8.csv")
