#### 2022-08-22 ####
## Tao Te Programming (TTP): Ch 24 Rise to the Occasion

## Today's Problem: Project Euler (PE) #1 (Multiples of 3 or 5)
### What we'll learn along the way:
#### R/RStudio Basics: arithmetic, assignment, types, subsetting
#### Branching (i.e. `if` statements)
#### Iteration (i.e. `for`/`while` loops)


#### Welcome to RStudio ####

## comments start with one or more number/pound signs
## keyboard shortcut for commenting multiple lines: cmd + shift + c
## code sections end with at least 4 pound signs


#### The Basics: Arithmetic ####

# you can do addition, exponentiation, modular arithmetic as you would expect
2+4
4^2
5 %% 2
5 %/% 2
5/2
3-4
16^(1/2)
16^1/2
(16^1)/2
sqrt(16)
exp(2)
exp(1)


#### The Basics: Assignment Operator ####
## types of vectors
## these are integers
## assignment is done with and "arrow" (i.e. `<-`) or an equals
divisor <- 3
dividend <- 5
quotient <- dividend %/% divisor
remainder <- dividend %% divisor

class(divisor)

## this is a logical/boolean (i.e. TRUE or FALSE)
## use double equals (i.e. `==`) to check if two objects are equal
dividend == divisor * quotient + remainder

## is 2 less than 4
2 < 4
## is 2 greater than 4
2 > 4

## is remainder calculated above less than the quotient
remainder < quotient

## is 2 less than (strictly so) than 2
2 < 2
## is 2 less than or equal to 2
2 <= 2

## does 1 equal 1
1 == 1
## does 1 not equal 1
1 != 1

## does `remainder` not equal `quotient`
remainder !=  quotient

## we can cast numerics as logicals
as.logical(1)
as.logical(0)

## we can cast logicals as numerics
as.numeric(TRUE)
as.numeric(FALSE)

## all non zero numbers get cast as true
as.logical(2)
as.logical(10)

#### The Basics: Subsetting ####
# To be visited on Wednesday

#### Random Sampling ####
# To be visited on Wednesday



#### PROJECT EULER: Problem #1 ####

## Problem 1: Multiples of 3 or 5
# 
# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
# 
# Find the sum of all the multiples of 3 or 5 below 1000.


#### Solution 1 ####
## with a `for` loop; with storing multiples
## To be reviewed in lab on Tuesday 

#### Solution 2 ####
## with a `for` loop; without storing multiples
## To be reviewed in lab on Tuesday 

#### Solution 3 ####
## with a `while` loop
## To be reviewed in lab on Tuesday 

#### Solution 4 ####
## vectorize and subset

## this "solution" is incorrect; do you see why?? 
### if not, investigate the first few values of the vector inside of the `sum()`
sum(c((1:999)[(1:999 %% 3) == 0], (1:999)[(1:999 %% 5) == 0]))

