#### The `apply` family of functions

## vectorizing
nums = 1:10
numsSq_1 = nums^2
numsSq_1

## the same as... 
numsSq_2 = numeric(length(nums))
for(iNum in 1:length(nums)){
    numsSq_2[iNum] = nums[iNum]^2
}
numsSq_2

square_number <- function(x){x^2}

numsSq_3 = numeric(length(nums))
for(iNum in 1:length(nums)){
    numsSq_3[iNum] = square_number(nums[iNum])
}
numsSq_3


## using sapply
numsSq_4 = sapply(nums, square_number)
## with an anonymous function
### if i only want to use the function once
numsSq_5 = sapply(nums, function(x){x^2})


## using lapply
## different from sapply in that it returns a list
lapply(nums, square_number)

## using apply??
apply(X = nums, FUN = square_number)
dim(nums)


twoDigNums = 10:99
pairOfTwoDigNums = expand.grid(twoDigNums, twoDigNums)
class(pairOfTwoDigNums)
dim(pairOfTwoDigNums)
### which margin is which?
## MARGIN = 1 returns vector of length 8100; i.e across the columns, i.e along the rows
apply(pairOfTwoDigNums, 1, prod)
apply(pairOfTwoDigNums, 1, sum)
## MARGIN = 2 returns vector of length 2; i.e along/down the columns, i.e across the rows
apply(pairOfTwoDigNums, 2, prod)

### can pass in other (i.e. other than the vector we are applying over) arugment to sapply
stringVector = c("apples", "oranges", "bananas")

library(stringr)
sapply(stringVector, str_sub, start = 1, end = 2)

str_sub(stringVector, start = 1, end = 2)

## Map; kind of like lapply but arguments are switched
Map(square_number, nums)
# lapply(square_number, nums)
lapply(nums, square_number)

#### Functional Programming ####
##  THE PIPE!!!!!
?`%>%`
library(magrittr)
## named after Belgium artist Rene Magritte

## `f(x)` is the same as `x %>% f()`
## `f(x, y)` is the same as `x %>% f(y)`

sum(nums)
nums %>% sum()

### Code Refactor: Making It Pretty ####

#### Refactoring Lab 3 ####
## the original version; 
library(stringr)
library(magrittr)
library(readr)

score_letter <- function(letter){
    which(LETTERS == letter)
}
score_letter("S")


namesFilePath = "Data/p022_names.txt"
names_raw = read_file(namesFilePath)
names_noQuotes = str_replace_all(names_raw, pattern = "\"", replacement = "")
names = unlist(str_split(names_noQuotes, pattern = ","))
names
sortedNames = sort(names)
nameLetters = str_split(sortedNames, "")
nameLetterScores = list()
for(iName in 1:length(nameLetters)){
    nameLetterScores[[iName]] = sapply(nameLetters[[iName]], score_letter)
}
nameLetterSums = sapply(nameLetterScores, sum)
nameScores = nameLetterSums * 1:length(names)
sum(nameScores)


## refactored version 1
### when refactoring chunk into single tasks
## use examples to check to make sure your refactor doesn't change anything important

namesFilePath = "Data/p022_names.txt"

read_names_txt <- function(namesFilePath) {
    names_raw = read_file(namesFilePath)
    names_noQuotes = str_replace_all(names_raw, pattern = "\"", replacement = "")
    names = unlist(str_split(names_noQuotes, pattern = ","))
    return(names)
}
read_names_txt(namesFilePath)

score_letter <- function(letter){
    which(LETTERS == letter)
}

sum_letter_scores <- function(name) {
    nameLetters = unlist(str_split(name, ""))
    nameLetterScores = sapply(nameLetters, score_letter)
    return(sum(nameLetterScores))
}
sum_letter_scores("COLIN")

score_names <- function(names) {
    sortedNames = sort(names)
    nameSums = sapply(sortedNames, sum_letter_scores)
    nameScores = nameSums * 1:length(names)
    return(sum(nameScores))
}

score_names(read_names_txt(namesFilePath))

## refactored version 2
## note the "define a variable and then use it in next line" phenomenom; 
### this is ripe for piping

namesFilePath = "Data/p022_names.txt"

read_names_txt <- function(namesFilePath) {
    namesFilePath %>% 
        read_file() %>% 
        str_replace_all('"', "") %>% 
        str_split(",") %>% 
        unlist() %>% 
        return()
}
read_names_txt(namesFilePath)


score_letter <- function(letter){
    which(LETTERS == letter)
}

sum_letter_scores <- function(name) {
    name %>% 
        str_split("") %>% 
        unlist() %>% 
        sapply(score_letter) %>% 
        sum() %>% 
        return()
}
sum_letter_scores("COLIN") == 53

score_names <- function(nameSums){
    nameSums * 1:length(nameSums)
}

sum_name_scores <- function(names){
    names %>% 
        sort() %>% 
        sapply(sum_letter_scores) %>% 
        score_names() %>% 
        sum() %>% 
        return()
}

sum_name_scores(read_names_txt(namesFilePath))

