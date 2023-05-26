#### TTP ####
## Chapter 32: Make Bricks Not Monoliths
### "A strong novice tendency is to build one thing that does everything. Do not do that. Better is to create a lot of small pieces that each do one thing."
### one reason is that this makes code more flexible/malleable/modular.

## Chapter 33: Write Opaque Code
### code should be opaque to other code. the only thing that should matter/be relevant are its inputs and outputs. 


#### Functions (cont) #### 
## function signatures: the name(s)/type(s) of the input(s) and the output type 
### demo python example; python does function signatures right

## the below shows an example of a general function singature
### the name comes first
### after the paretheses, the name of each input is provided along with
### the type and length of the input (e.g. numeric(10), character(1)) after the colon
### as well as the default value after the equal; 
#### the parenthese here indicate that the default is optional. 

## function_name(
##     variableName_1: variableType_1 (= default_1),
##     variableName_2: variableType_2 (= default_2),
##     ...
##     variableName_k: variableType_k (= default_k)
## ){
##    body of function
##    return(
##        outputName: outputType
##    )
## }

### in R checking types must be done explicitly
## the signature for `get_fibonaccis()` is:
### get_fibonaccis(n: integer(1)) -> integer(n)

## intializes a vector, of type "integer" of length 1/4
integer(1)
integer(4)

## intializes a vector, of type "character" of length 5
charVect = character(5)
class(charVect)
length(charVect)




## redone with checking of input types 
get_fibonaccis <- function(n){
    ## check that n is an integer
    if(n != round(n)){
        stop("`n` needs to be an integer")
    }
    
    ## check that n is positive
    if(n < 0){
        stop("`n` needs to be positive")
    }
    
    if(n == 0){
        return(numeric(0))
    }
    
    if(n <= 2){
        return(c(1, 1)[1:n])
    }
    
    fibonnacis = numeric(n)
    fibonnacis[1] = 1
    fibonnacis[2] = 1
    
    for(i in 3:n){
        fibonnacis[i] = sum(fibonnacis[(i-2):(i-1)])
    }
    return(fibonnacis)
}

get_fibonaccis(1)
get_fibonaccis(2)
get_fibonaccis(5)

get_fibonaccis(3.1)
get_fibonaccis(pi)

get_fibonaccis(-2)

get_fibonaccis(0)


#### Stringr ####
## the tidyverse package for string manipulation
library(stringr)
## another package that can help with string manipulation
library(stringi)

## a string to play around with
aString = "Umm, when is lunch?"
## it is a character of length 1
typeof(aString)
length(aString)

## Subsetting Strings
## goes from first to last (i.e. not actually a "proper subset")
str_sub(aString, start = 1, end = -1)


## select from start to end (inclusively)
str_sub(aString, start = 1, end = 4)
str_sub(aString, start = 1, end = 5)
str_sub(aString, start = -5, end = -1)

## this is a different type of subsetting; 
### it still returns a character of the same size

## it can work for multiple strings as well
multipleStrings = c(
    "Umm, when is lunch",
    "Really? We just ate breakfast!",
    "I know... but I'm a growing turtle",
    "Lunch isn't for another week.",
    "Ohh pebbles."
)

str_sub(multipleStrings, start = 1, 6)


## Length of a String
## Q: how do I get the "actual" length of the string (i.e. the number of characters)
## A: with `str_length()`
str_length(aString)
str_length("abcde")
str_length(multipleStrings)

## duhh.
str_length(str_sub(multipleStrings, 1, 6))


## Splitting Strings by Patterns
### this will split on a space; returning the words + punctuation
splitAString = unlist(str_split(aString, pattern = " "))
typeof(splitAString)
length(splitAString)
## Count the number of words 
numOfWords = length(splitAString)

splitMultipleStrings = str_split(multipleStrings, pattern = " ")
## returns a list??
class(splitMultipleStrings)


## Replace Characters with Other Characters
aString
str_replace(aString, ",", "")

## replacing first occurrence of a vowel with X
str_replace(aString, "[aeiou]", "X")
str_replace(aString, "[aeiouAEIOU]", "X")

## replacing all occurrence of a vowel with X
str_replace_all(aString, "[aeiou]", "X")
str_replace_all(aString, "[aeiouAEIOU]", "X")
