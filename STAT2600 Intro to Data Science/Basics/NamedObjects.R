#### Named Objects ####
## we saw how to name an object in the "Assignments" module
## now we want to discuss how to name other components of an object
anUnnamedVector = 1:3

## give a vector names with the `names()` function
aNamedVector = 1:3
names(aNamedVector) = c("first", "second", "third")

## notice the different ways these print
anUnnamedVector
aNamedVector

## we can also name a vector with the `c()` function
anotherNamedVector = c(first = 1, second = 2, third = 3)

# you can use quotes if you really want to, but it doesn't matter to R
yetAnotherNamedVector = c("first" = 1, "second" = 2, "third" = 3)

identical(anotherNamedVector, yetAnotherNamedVector)

## unnamed vectors have `NULL` as their names; you can remove the names of a vector by assigning them to `NULL`
names(anUnnamedVector)
names(aNamedVector)

names(aNamedVector) = NULL

## a named vector no more
aNamedVector




#### Subsetting with Names ####
## one of the nice things about names is that we can use them to 
