#### Lists ####
## Lists are so-called "generic vectors" and differ from atomic vectors in one crucial way: their elements don't need to be the same type. For example, a list can contain a numeric as it's first element, a character as it's second element, and another list as it's third element. 

## as a specific example, we might have a list with the following elements:
### 1. 23
### 2. c("awk:, "ward")
### 3. list(1:2, 2:4, 3:6)

### the first element is a numeric of length 1, the second a character vector of length 2 and the last is a list of lenght three, where each element is itself a numeric vector; moral of the story these are all wildly different objects!

## we can create a list with the `list()` function
aDiverseList = list(23, c("awk", "ward"), list(1:2, 2:4, 3:6))
aDiverseList

## the length of the list is the number of elements it contains; we can of course use the `length()` function to return the length of a list
length(aDiverseList)

## we can use lengths to get the lengths of each of the elements in a list
lengths(aDiverseList)

## sometimes we want to create an empty list; this can be done in at least a few  ways. 
anEmptyList = list()
anotherEmptyList = vector(mode = "list", length = 0)
yetAnotherEmptyList = as.list(c())

length(anEmptyList)


#### Unlisting ####
## sometimes we have a list where all vectors are the same type (but perhaps different lengths) and we want to create one long atomic vector. 

aListOfWords = list(
    c("Two", "roads",  "diverged", "in", "a", "yellow", "wood"),
    c("And", "sorry", "I", "could", "not", "travel", "both"),
    c("And", "be", "one", "traveler", "long", "I", "stood"),
    c("And", "looked", "down", "one", "as", "far", "as", "I", "could"),
    c("To", "where", "it", "bent", "in", "the", "undergrowth")
)
typeof(aListOfWords)
length(aListOfWords)
lengths(aListOfWords)

## in this case we can use `unlist()` to "flatten" the hierarchy of a list
allTheWords = unlist(aListOfWords)
allTheWords
typeof(allTheWords)
length(allTheWords)






