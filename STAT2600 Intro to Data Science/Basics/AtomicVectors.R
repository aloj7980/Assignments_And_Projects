#### Atomic Vectors ####
## vectors can be thought of as ordered lists
### for instance, you might have a vector called "mammals" that has elements (i.e. members)
#### 1. "cat"
#### 2. "dog" 
#### 3. "squirrel"
#### 4. "mouse"

### in R we might create this vector with the `c()` (for combine) function 
mammals = c("cat", "dog", "squirrel", "mouse")

## we can return/print a vector by calling its name
mammals

## or we can print with the print function
print(mammals)

## order matters! for instance this is considered a different vector even though it has the same elements
mammals2 = c("mouse", "cat", "squirrel", "dog")
mammals2
identical(mammals, mammals2) # returns equal if objects are identical

## what distinguishes atomic vectors from other kinds of vectors is that the elements of an atomic vector must be all of the same type. 

#### Atomic Vector Types ####
## there are 6 "atomic types" in R:  
### 1. logical
### 2. integer
### 3. double
### 4. complex
### 5. character
### 6. raw

## we won't really use complex and "raw" vectors all that much
### the other 4 are use a lot!

## here is an example of each: 
### notice that for an integer we must append the number with "L"
logicalVector = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
integerVector = c(-2L, 0L, 1L, 2L, 5L, 6L, 8L)
doubleVector = c(2.4, 1, 5.2, -6.0, 0.42)
characterVector = c("cat", "dog", "squirrel", "mouse")

## strictly speaking, the "L" is important 
wannabeIntegerVector = c(-2, 0, 1, 2, 5, 6, 8)
identical(integerVector, wannabeIntegerVector)

## you can check the type of vector with the `typeof()` function
typeof(logicalVector)
typeof(integerVector)
typeof(doubleVector)
typeof(characterVector)

## now we can see why these objects aren't identical
typeof(integerVector)
typeof(wannabeIntegerVector)

## you can also cast a double as an integer with the function `as.integer()`
integerVector2 = as.integer(c(-2, 0, 1, 2, 5, 6, 8))
identical(integerVector, integerVector2)

## tbh, while we work with integers all of the time (indices, lengths, dimensions are all integers), it's very rarely important that R store them as an integer. for our purposes, we will conceptually think of (and audibly refer to) integers and doubles as "numeric". 

#### Length of a Vector ####
## in addition to type, another fundamental attribute of an atomic vector is its length; this is just the total number of elements in the vector

## as you would expect, the `length()` function will return the length of a vector
mammals
length(mammals)


#### Constructing Atomic Vectors ####
## these vectors are all the same
c(FALSE, FALSE, FALSE)
vector(mode = "logical", length = 3)
logical(3) 
rep(FALSE, 3) ## `rep()` is a handy function that repeats its first arguments, the second argument number of times

## we often use `logical()` to preallocate a vector that will be populated later; `numeric()` and `character()` are the equivalent functions for their respective types

## notice that these are populated with the appropriate "nothing" element: `FALSE` for logicals, `0` for numerics, and the empty string `""` for characters
logical(10) 
numeric(10)
character(10)

#### Logical Vectors ####
## logical vectors will often come from comparisons
c(1, 2, 3, 4, 5) < c(5, 4, 3, 2, 1)
c(1, 2, 3, 4, 5) <= c(5, 4, 3, 2, 1)
c(-2, 0, -3, 2) < 0

#### Numeric Vectors ####
## numerics can be created a number of ways
## the colon can be used between two numerics (most often integers) to create a vector of numerics starting at the first and incrementing by one until it does not exceed the second

1:10
1:10.2
1.3:10.2

## the `seq()` function will do the same with two arguments
seq(1, 10)
seq(1, 10.2)
seq(1.3, 10.2)

## passing a third argument to `seq()` will increment by that amount
seq(1, 10, 1)
seq(0, 1, .1)
seq(0, 10, 2)


#### Character Vectors ####
## characters come up more than you might think

## two nice character vectors that are build into R are the `letters` and `LETTERS` constants that contain the lowercase and uppercase letters of the alphabet respectively
letters

LETTERS


#### Coercion in Atomic Vectors ####

## you can try to make a vector of containing different types
c(TRUE, 4L, 3.2, "bogey")

## but R won't let you actually create such a vector; rather it will "coerce" all the elements to the same "least common denominator" type
## coersion rule goes logical -> integer -> numeric -> complex -> character

## notice `FALSE` gets coerced to `0` and `TRUE` is coerced as `1`
coercedToInteger = c(FALSE, -2L, TRUE, 4L)
coercedToInteger
typeof(coercedToInteger)

coercedToDouble = c(4L, 3.1, 0L, -2.4)
coercedToDouble
typeof(coercedToDouble)

## notice the quotes here; `"3.1"` does not equal `3.1`
coercedToCharacter = c(3.1, "cat", FALSE, -2L)
coercedToCharacter
typeof(coercedToCharacter)
