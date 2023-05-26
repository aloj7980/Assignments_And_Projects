#### TTP ####
## forgot my book today :/

## Reproducible examples: a couple of useful conversations/posts

## https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example
# https://xiangxing98.github.io/R_Learning/R_Reproducible.nb.html

dfConcat = data.frame(
    LogCol = logical(nRows),
    CharCol = rep("words", nRows),
    NumCol = numeric(nRows)
)
dfConcat
dput(dfConcat)

#### Homework 1 (cont) ####
## getting the signatures, correct

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
compute_text_file_numeric_summary(textFile){
    if(!is.character(textFile)){
        stop("`textFile` should be type character.")
    }
    
    nLines = length(textFile)
    # nBlankLines = 0
    
    
    return(
        ## PLACEHOLDER
        numeric(
            nLines,
            0, 
            0,
            0,
            0,
            0
        )
    )
}
# compute_text_file_word_counts(textFile: character(n)) -> data.frame(kx2): takes a text file as its input and outputs a dataframe with k rows and 2 columns where k is the number of distinct “words”. Here “words” include English word, variable names, function names, or any string that starts with a letter and contains only alpha-numerics, periods, and underscores. The first column will consist of the different words and the second columns will be the frequency with which the word appears in the text file. The names of the columns should be Word and Count and it should be sorted by frequency in descending order.
compute_text_file_word_counts <- function(textFile){
    # check textFile: character(n)
    if(!is.character(textFile)){
        stop("`textFile` should be type character.")
    }
    
    ## body of function
    wordFromTextFile = extract_words_from_lines(textFile)
    
    ## count how many of each and return them
    
    return(
        ### PLACEHOLDER
        data.frame(
            Word = letters,
            Count = numeric(length(letters))
        )
    )
}

#### Data Frames vs Matrices ####
## data frames can hold non-homagenous columns; but each column must be homogenous
nRows = 8
dfConcat = data.frame(
    LogCol = logical(nRows),
    CharCol = rep("words", nRows),
    NumCol = numeric(nRows)
)
dfConcat
type(dfConcat)

cbindConcat = cbind(
    LogCol = logical(nRows),
    CharCol = rep("words", nRows),
    NumCol = numeric(nRows)
)
cbindConcat
type(cbindConcat)


#### Timing Experiments: Making It Fast ####
## Make it work. Make it fast. Make it pretty.

#### Experiment 1: Reading Names ####
namesFilePath = "Data/p022_names.txt"

library(readr)
library(stringr)

startTime1 = Sys.time()
names_raw = read_file(namesFilePath)
names_noQuotes = str_replace_all(names_raw, pattern = "\"", replacement = "")
names1 = unlist(str_split(names_noQuotes, pattern = ","))
endTime1 = Sys.time()
endTime1 - startTime1

startTime2 = Sys.time()
names2 = scan(namesFilePath, what = "", sep = ",", na.strings = "")
endTime2 = Sys.time()
endTime2 - startTime2

startTime3 = Sys.time()
names3 = unlist(as.vector(read.delim(namesFilePath, header = FALSE, sep = ",")))
endTime3 = Sys.time()
endTime3 - startTime3


all(names1 == names2)
all(names1 == names3)


#### Experiment 2: Multiplying Numbers ####
# multiplying 3-digit numbers in different ways
twoDigNumbers = 10:99

## `for` loop with appending
start = Sys.time()
products_1 = numeric()
for(first in twoDigNumbers){
    for(second in twoDigNumbers){
        products_1 = c(products_1, first*second)
    }
}
end = Sys.time()
duration_1 = end - start


## `for` loop with allocation
start = Sys.time()
products_2 = numeric(length(twoDigNumbers)^2)
counter = 1
for(first in twoDigNumbers){
    for(second in twoDigNumbers){
        products_2[counter] = first*second
        counter = counter + 1
    }
}
end = Sys.time()
duration_2 = end - start

## matrix mutliplication
start = Sys.time()
products_3 = as.vector(
    twoDigNumbers %*% t(twoDigNumbers)
)
end = Sys.time()
duration_3 = end - start


## `expand.grid` and apply()
start = Sys.time()
products_4 = apply(
    expand.grid(twoDigNumbers, twoDigNumbers),
    1, prod
)
end = Sys.time()
duration_4 = end - start

c(
    duration_1,
    duration_2,
    duration_3,
    duration_4
)
