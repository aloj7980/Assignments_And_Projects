#### Subsetting ####
## for creating vectors see the 
## how do I access particular members of a vector? 

#### Atomic Vectors ####
## you can subset an atomic vector by using square brackets `[]`
someNumbers = c(12, 13, 14, 15, 16, 17)

## you can pass in the index/position of the elements you want to access
someNumbers[1] # returns first element in vector
someNumbers[3] # returns the third element
someNumbers[c(1, 3)] # returns the first and third element

## if we try to subset an atomic vector "out-of-bounds" it returns an `NA`
someNumbers[12]

## you can also pass in a logical vector the same length as the vector you want to subset; it will return the elements that correspond to the TRUE values
someNumbers[c(T, F, F, F, F, F)] ## returns the first element
someNumbers[c(T, F, T, F, F, F)] ## returns the first element


## what happens if your logical isn't the same length as the vector you want to subset? do we get an error?? 
someNumbers[c(T, F, F, F, F)] # logical of length 5
someNumbers[c(T, F, F, F)] # logical of length 4
someNumbers[c(T, F, F)] # logical of length 3
someNumbers[c(T, F)] # logical of length 3

## no... it doesn't return an error. it's actually not obvious what's going on here. let's look at a few other examples...
someNumbers[c(T, T, F)]
someNumbers[c(T, F, T)]

## R will actually "recycle" the logical vector until it's the appropriate length; this recycling behavior occurs rather frequently in R.
identical(someNumbers[c(T, F)], someNumbers[c(T, F, T, F, T, F)])

## another example of recycling with addition
identical(
    someNumbers + c(-10, 10), 
    someNumbers + c(-10, 10, -10, 10, -10, 10)
)

## yet another way we can subset is with names; see the `NamedObjects.R` module for more on this. 



#### Lists ####
## subsetting lists works much the same; we can subset certain elements by using square brackets `[]`
aListOfObjects = list(1:10, letters[3:5], TRUE, seq(0, 1, .25))
aListOfObjects

aListOfObjects[1] ## returns the first element of the list 
aListOfObjects[3] ## returns the third element of the list
aListOfObjects[c(1, 3)] ## returns the first and third elements of the list

## different from atomic vectors, if we try to subset a list "out-of-bounds" we get `NULL` (rather than `NA`)
aListOfObjects[12]

## one important thing to note is that we get a list back with a single set of square brackets
typeof(aListOfObjects[1])
typeof(aListOfObjects[c(1, 3)])

## this makes sense when we want to subset multiple elements of a list; after all they aren't guaranteed to be the same type so we may need it to be returned as a list!

## but often when we are subsetting just one element in a list we want the element itself, rather than a list of length one containing the element; perhaps we want to further subset 

## this doesn't return the 6th element from the first element in the list; rather we get `NULL` because this is subsetting the list and there isn't a 6th element in this list
aListOfObjects[1][6]

## to subset in this way we use to two square brackets `[[]]`
aListOfObjects[[1]] ## returns the first element of the list (not as a list)
typeof(aListOfObjects[[1]])

## now that this itself is an atomic vector, we can subset it with single square bracket
aListOfObjects[[1]][6] # returns the 6th element from the first element in the list

## subsetting can get really weird if you want it to
aListOfObjects[2:4][3][[1]][2:4][2]

## i wouldn't recomment this sort of thing... but if you CAN follow what's going on here then certainly you can follow the much more straightforward subsetting  
aListOfObjects[[4]][3]

## logicals can also be use like in atomic vectors
aListOfObjects[c(TRUE, FALSE, TRUE, FALSE)] ## selects first and thrid elements from the list (as a list)

## they don't work quite as intuitively as you might like though
aListOfObjects[[c(TRUE, FALSE, FALSE, FALSE)]] ## does not select the first element (not as a list)

aListOfObjects[c(TRUE, FALSE, FALSE, FALSE)][[TRUE]] ## i guess this works

## once again, yet another way we can subset lists is with names; this is probably more popular with lists than atomic vectors; again, see the `NamedObjects.R` module for more on this. 



