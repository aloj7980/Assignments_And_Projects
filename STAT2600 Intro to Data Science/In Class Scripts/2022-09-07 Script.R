#### TTP ####
## Ch 44: Don't Borrow, Steal

## "If you take code of a methodology from somewhere else, then tamp it in properly so it fits precisely the hole you have."

#### Homework 1 ####
## my recommendation for getting it started
### copy in the function signature; edit to ensure type checks; don't change the name of anything

### will look something like this to start
read_text_file <- function(textFilePath, withBlanks = TRUE, withComments = TRUE){
    # check that `textFilePath`: character(1)
    if(!is.character(textFilePath) | length(textFilePath) != 1){
        stop("`textFilePath` must be a character of length 1.")
    }
    
    # check that `withBlanks`: logical(1)
    
    # check that `withComments`: logical(1)
    
    ## body of the function
    
    ## need to change this; just a placeholder for now
    return(character(42))
}     

var1 = read_text_file("thisIsASingleCharacter")
## no output when there is an error 
var2 = read_text_file(c("thisIsNot", "ASingleCharacter"))
var2


#### Lists #### 
## vector that can contain disparate typed elements
## why `string_split()` returns a list()

atomicVector = c(1, FALSE, "words")
typeof(atomicVector)
atomicVector
## when you try to use mixed/disparate types in an atomic vector, R will (try to) coerce them all to be the same... worst case, everything can be a character. 

## another example of coercing
c(1, FALSE)

## sometimes we do want different types in a vector; enter lists
listVector = list(1, FALSE, "words")
typeof(listVector)
## lists are indexed with two square brackets (i.e. [[]])
## to get the first element from a list do...

## take the first element and return just that
listVector[[1]]
typeof(listVector[[1]])
listVector[1]
typeof(listVector[1])

## take first and second element from list and return list
listVector[1:2]
typeof(listVector[2])


## another example
listVector2 = list(rep(1, 3), FALSE, letters)

## this returns a character vector
listVector2[[3]]
all(listVector2[[3]] == letters)

## this returns a single logicl
listVector2[[2]]

list(
    5, 
    3, 
    FALSE
)

## an example with `str_split`
source("Labs/Lab3_20220906/pe22.R")
pe22Names <- read_names_txt("Data/p022_names.txt")
pe22Names
nameLettersList = str_split(pe22Names, "")

## what is the type of this object
str_split("STEPHEN", "")[[1]]

typeof(nameLettersList)

for(nameLetters in nameLettersList[1:10]){
    print(nameLetters)
    print(length(nameLetters))
}

rss()
nameLettersList_lenThree = str_split_fixed(pe22Names, "", n = 3)


#### String Manipulation ####
## `extract_words_from_line(line: character(1)) -> character(n)`
### a function that takes in a line of code and outputs the "words" as defined in hw1

### Stealing Code
### cheat sheet
### documentation (i.e. "Help" pane)
### trial and error

## these are the examples from the help documentation
shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
str_extract(shopping_list, "[a-z]+")
str_extract(shopping_list, "[a-z]{1,4}")
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

# Extract all matches
str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")

source("Homeworks/HW1/hw1.R")
codeLines = read_text_file(
    "LectureScripts/2022-09-02 Script.R",
    noBlanks = TRUE
)[21:45]

## now we need to "tamp it in" for our use case

source("Homeworks/HW1/hw1.R")
textFile = read_text_file("Homeworks/HW1/hw1.R")
matchesInLine = unlist(str_extract_all(textFile, "[a-zA-Z1-9_.]+"))
startWithLetter = str_detect(str_sub(matchesInLine, 1, 1), "[a-zA-Z]") 

## a slightly different way using `%in%`
# startWithLetter = str_sub(matchesInLine, 1, 1) %in% c(letters, LETTERS)

## when subsetting with at boolean, it will return only values corresponding to TRUE
matchesInLine[startWithLetter]

cbind(
    matchesInLine,
    str_sub(matchesInLine, 1, 1),
    str_detect(str_sub(matchesInLine, 1, 1), "[a-zA-Z]") 
)


## the function signature
# extract_words_from_lines(textFile: character(n)) -> character(m)
extract_words_from_lines <- function(textFile){
    matchesInLine = unlist(str_extract_all(
        textFile, "[a-zA-Z1-9_.]+"))
    startWithLetter = str_detect(str_sub(
        matchesInLine, 1, 1), "[a-zA-Z]") 
    return(matchesInLine[startWithLetter])
}


### helpful regex website
## https://regex101.com/








