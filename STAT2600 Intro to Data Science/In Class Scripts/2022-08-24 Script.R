#### TTP ####

## Tao Te Programming (TTP): Chapter 5: Solve the Problem

## the four steps for general problem solving 
### 1. List the starting ingredients
### 2. State the desired results
### 3. Break the journey from step 1 to step 2 into subproblems
### 4. Put the subproblem solutions together

## it's a recursive algorithm, the same four steps can be applied to subproblems, and on their subproblems.


## Today's problem: randomly sample a student to call upon

### What we'll learn along the way:
#### reading csv's
#### subsetting
#### random sampling


#### Reading a CSV ####
### `read.csv` will read a csv into your environment
### for more info on `read.csv` (or any function) execute `help("read.csv")` 
#### or search it in the "Help" tab in the bottom right pane.
class_df <- read.csv(file = "ClassRoster.csv")

### `View()` allows you to view your dataframe like you might in excel/spreadsheets
View(class_df)


#### Subsetting ####
## subsetting is done with square brackets (i.e. `[]`)
## the column names of the `class_df`
names(class_df)

## subsetting the entire first row of `class_df`
class_df[1, ]

## subsetting the entire first column of `class_df`
class_df[, 1]
## different ways to subset the first column
class_df[, "Name"]
class_df$Name

## assinging the first columns as the variable `student_names`
student_names <- class_df$Name
student_names

## subsetting the `student_names` vector
## subsetting the first
student_names

## subsetting the 23rd
student_names[23]

## subsetting the first two names
student_names[1:2]

## subsetting the first, third and nineth names
student_names[c(1, 3, 9)]

## the number of students
n_students <- length(student_names)

## subsetting the last 5 students
student_names[(n_students-5+1):n_students]


#### Random Sampling ####
## the `sample()` function will randomly sample some number of elements from a set 
## if you don't pass in the `size` for your sample it will return a random sample of all the elements (i.e. a permutation)
sample(1:10)
sample(1:10, size = 10)

## passing arguments without calls to input names (not recommended); without use of input names order matters!
## behaves same as `sample(x = 1:10, size = 10)`
sample(1:10, 10)
## behaves same as `sample(x = 1:10, size = 10, replace = FALSE)`
sample(1:10, 10, FALSE)
## behaves same as `sample(x = 1:10, size = 10, replace = TRUE)`
sample(1:10, 10, TRUE)
## wrong order; does not behave same as `sample(x=1:10, replace=FALSE, size=10)
sample(1:10, FALSE, 10)
sample(x=1:10, replace=FALSE, size=10)

## sampling just one element from the set `x`
sample(1:10, size=1)

## creates a deck of cards where suit is clubs=1, diamonds=2, hearts=3, spades=4
deck_of_cards = expand.grid(1:13, 1:4)
names(deck_of_cards) = c("Rank", "Suit")

## deals random 5-card hand
deck_of_cards[sample(1:52, size = 5), ]


### Putting it together
## samples 
student_names[sample(1:n_students, size=1)]
class_df[sample(1:n_students, size=1), 1:2]

## this throws an error "incorrect number of probabilities"
## `prob` input in sample; needs to be a vector the same size as `x`
sample(x=1:n_students, size=1, prob=1/n_students)

## use `rep()` to repeat same probability correct number of times
sample(
    x=1:n_students, 
    size=1, 
    prob=rep(1/n_students, n_students)
)

#### Sum of Two Dice ####
## an example where you would use non-equal probabilities

## assign different probabilities to outcomes
sample(2:12, size = 1, prob = c(1:6, 5:1)/36) 
## `prob` input doesn't need to sum to 1
sample(2:12, size = 1, prob = c(1:6, 5:1)) 

## rolling many dice
nRolls = 5*10^3
hist(
    sample(2:12, size = nRolls, replace=TRUE, prob = c(1:6, 5:1)),
    breaks = (1:12 + 1/2),
    main = paste("Histogram for", nRolls, "Sums of Two Dice Rolls"),
    xlab = "Sum of Two Die",
)


