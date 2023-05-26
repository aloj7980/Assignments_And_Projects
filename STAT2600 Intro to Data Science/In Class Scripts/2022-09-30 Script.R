#### Homework 2 ####
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(rlang)

replace_value_with_na <- function(x, value){
    x[x == value] = NA
    return(x)
}

remove_value <- function(x, value){return(x[x!=value])}

netflixDf <- read_csv("Homeworks/HW2/NetflixMoviesShows.csv")

directorNames = netflixDf$director %>% 
    replace_value_with_na("Not Given") %>% 
    unique() %>% 
    sort()

directorDict = 1:length(directorNames)
names(directorDict) = directorNames

### `DirectorKey` column with a dictionary
netflixDf %>% 
    mutate(
        DirectorKey = directorDict[director]
    ) 


### `DirectorKey` column with a join

netflixDf %>% 
    left_join(
        personDf, 
        by = c("director" = "Name")
    ) %>% as_tibble()

### writing csv without index column
personDf %>% write_csv("person_df.csv")

personDf %>% write.csv("person_df_2.csv", row.names = FALSE)


#### Text Modeling ####

library(readr)
library(stringr)
library(dplyr)


geh_lines_raw = readLines("Data/GreenEggsAndHam.txt", warn = FALSE)

remove_blank_lines <- function(x){
    x[str_trim(x) != ""]
}

## cleaning this dataset
### fun fact: there are 50 words in green eggs and ham.

## cleaning up the lines 

gehLines = geh_lines_raw %>% 
    remove_blank_lines %>% 
    str_replace_all("-", " ") %>% 
    str_remove_all("[:punct:]") %>% 
    tolower() %>% 
    str_trim()
length(gehLines)

gehWords = gehLines %>% 
    str_split(" ") %>% 
    unlist() %>% 
    unique() %>% 
    sort()

length(gehWords)

### one hot encode
allWords = gehWords
one_hot_encode_line <- function(allWords, lineWords){
    as.numeric(allWords %in% lineWords)
}

lines = geh_lines
one_hot_encode_lines <- function(allWords, lines){
    lineWordsList = lines %>% str_split(" ")
    oheMatrix = t(sapply(lineWordsList, 
           one_hot_encode_line, 
           allWords = allWords))
    colnames(oheMatrix) = allWords
    return(oheMatrix)
}

gehOheMatrix = one_hot_encode_lines(
    allWords = gehWords, 
    lines = gehLines)



### other ways of encoding text data

### instead of if a word appears (i.e TRUE/FALSE), 
#### count number of occurences

### could look at single words and pairs of words
#### this is sometimes called n-gram encoding

### TFIDF term frequency–inverse document frequency



gehLines = textLines %>% 
    remove_blank_lines() %>% 
    str_replace_all("-", " ") %>%  
    str_remove_all("[:punct:]") %>% 
    str_trim() %>% 
    tolower()

gehWords = gehLines %>% 
    str_split(" ") %>% 
    unlist() %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    names() %>% sort()

#### Exploratory Data Analysis ####

houseDf = read_csv("Data/house-prices-advanced-regression-techniques/train.csv")
houseDf

