require(stringr)
library(dplyr)
read_text_file <- function(textFilePath, withBlanks = TRUE, withComments = TRUE){
  #check textFile: character(n)
  if(!is.character(textFilePath)){
    stop("`textFilePath` should be type character.")
  }
  lines = readLines(textFilePath)
  isLineKept = rep(TRUE,length(lines))
  for(n in 1:length(lines)){
    blankCheck = TRUE
    for(x in 1:str_length(lines[n])){
      if(str_length(lines[n]) == 0) break
      if(str_sub(lines[n],x,x) != " "){
        blankCheck = FALSE
        break
      }
    }
    if(str_sub(lines[n],1,1) == "#" && !withComments){
      isLineKept[n]=FALSE
    }else if(blankCheck && !withBlanks){
      isLineKept[n]=FALSE
    }else{
      isLineKept[n]=TRUE
    }
  }
  return(lines[isLineKept])
}

# compute_text_file_summary(textFile: character(n)) -> list(2): takes in a text file (a character vector where each element is a line of text/code) as an input and returns a named list of length 2 as the output; the output lcist will contain the outputs of the below defined compute_text_file_numeric_summary() and compute_text_file_token_counts() functions with names NumericSummary and WordCounts respectively.
compute_text_file_summary <- function(textFile){
  #check textFile: character(n)
  if(!is.character(textFile)){
    stop("`textFile` should be type character.")
  }
  
  return(
    list(
      NumericSummary = compute_text_file_numeric_summary(textFile), 
      WordCounts = compute_text_file_word_counts(textFile)
    )
  )
}

# compute_text_file_numeric_summary(textFile: character(n)) -> numeric(6): takes a text file as its input and outputs a numeric of length 6 with the following characteristics of the text file:
#     1. the number of lines (from the signature we know this is just n)
#     2. the number of blank lines (i.e. lines that contain nothing or only whitespace) 
#     3. the number of lines that are comments (i.e. lines that starts with “#”)
#     4. the total number of characters in the text file
#     5. the median line length (i.e. the median number of characters per line)
#     6. the max line length (i.e. the max number of characters in a line)
compute_text_file_numeric_summary <- function(textFile){
  if(!is.character(textFile)){
    stop("`textFile` should be type character.")
  }
  numericSummary = c(0,0,0,0,0,0)
  charsInLine = rep(0,length(textFile))
  for(n in 1:length(textFile)){
    blankCheck = TRUE
    for(x in 1:str_length(textFile[n])){
      if(str_length(textFile[n]) == 0) break
      if(str_sub(textFile[n],x,x) != " "){
        blankCheck = FALSE
        break
      }
    }
    numericSummary[1] = numericSummary[1] + 1
    if(blankCheck){
      numericSummary[2] = numericSummary[2] + 1
    }else if(str_sub(textFile[n],1,1) == "#"){
      numericSummary[3] = numericSummary[3] + 1
    }
    chars = str_length(textFile[n])
    numericSummary[4] = numericSummary[4] + chars
    charsInLine[n] = chars
  }
  numericSummary[5] = median(charsInLine)
  numericSummary[6] = max(charsInLine)
  return(numericSummary)
}
extract_words_from_lines <- function(textFile){
  matchesInLine = unlist(str_extract_all(
    textFile, "[a-zA-Z1-9_.]+"))
  startWithLetter = str_detect(str_sub(
    matchesInLine, 1, 1), "[a-zA-Z]") 
  return(matchesInLine[startWithLetter])
}
# compute_text_file_word_counts(textFile: character(n)) -> data.frame(kx2): takes a text file as its input and outputs a dataframe with k rows and 2 columns where k is the number of distinct “words”. Here “words” include English word, variable names, function names, or any string that starts with a letter and contains only alpha-numerics, periods, and underscores. The first column will consist of the different words and the second columns will be the frequency with which the word appears in the text file. The names of the columns should be Word and Count and it should be sorted by frequency in descending order.
compute_text_file_word_counts <- function(textFile){
  # check textFile: character(n)
  if(!is.character(textFile)){
    stop("`textFile` should be type character.")
  }
  words = extract_words_from_lines(textFile)
  distinctWords = unique(words)
  counts = rep(0,length(distinctWords))
  for(word in words){
    index = 0
    for(n in 1:length(distinctWords)){
      if(word == distinctWords[n]){
        index = n
      }
    }
    counts[index] = counts[index] + 1
  }
  myFrame = data.frame(
    Word = distinctWords,
    Count = counts
  )
  newFrame = arrange(myFrame,desc(Count))
  return(newFrame)
}


