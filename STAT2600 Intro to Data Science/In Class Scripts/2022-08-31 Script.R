#### TTP ####
## Chapter 73: King Your User
## the utility of a great example...

## compare these examples; 
### do they capture the essence of `sample()`
### do they capture the entirety of `sample()`

coinToss = sample(c("Heads", "Tails"), size = 1)
coinTosses = sample(c("H", "T"), replace = TRUE, size = 10)
nRolls = 20
diceRolls = sample(x = 1:6, size = nRolls, replace = TRUE)
sumOfTwoDieRolls = sample(
    x = 2:12, size = nRolls, 
    replace = TRUE, prob = c(1:6, 5:1)
)
shuffledAlphabet = sample(x = letters)
tileFreq = c(
    9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2, 
    6, 8, 2, 1, 6, 4, 6, 4, 2, 2, 1, 2, 1
)
scrabbleHand = sample(
    letters, size = 7, 
    replace = TRUE, prob = tileFreq
)

## a fun website for examples
### https://www.mayin.org/ajayshah/KB/R/html/b1.html


#### Lab 2 Questions ####
## Q: what does the `all()` do?
### A: it takes a vector of logicals and returns true if they are all true
### A+: `any()` takes a vector of logicals and returns true if any of them are true
### for a logical of size two, `all() is the same as `&` and `any()` is the same as `|`
logicals = c(TRUE, FALSE)
all(logicals)
any(logicals)

logicals = c(TRUE, TRUE)
all(logicals)
any(logicals)

logicals = rep(TRUE, 100)
all(logicals)
any(logicals)

logicals = c(rep(TRUE, 100), FALSE)
all(logicals)
any(logicals)

## a cousin of `all()` is `any()`
## de-morgans (sp?) law relates `&` (and) `|` (or) `any()` and `all()` and `!` (not)


#### Projects in RStudio ####
## setting up a project
### projects are nice for many reasons; but basically they help organize your work

## your working directory; this is where R references things from; R's "home page" 
getwd()
## you can set your working directory with `setwd()`... not recommended; 
### instead navigate with relative file paths

## `list.files()` is a handy function for listing the files in a directory/folder
## list files in working directory
list.files()

## list files in folder Lecture outlines (this folder must be present for this to work)
list.files("LectureOutlines")

## list files in folder inside folder inside folder (folders all the way down)
list.files("Labs/Lab1_20220823/submissions")

## list files in parent folder of (i.e. folder which contains) working directory
list.files("..")


## files paths come in two different varieties; absolute and relative
## uses an absolute path; on your computer/environment where is this file
classRosterDf_dot = read.csv("/Users/MiSelf2/My Drive/STAT 2600 (F22)/Data/ClassRoster.csv")

## uses a relative path; relative to your working directory where is this file
classRosterDf_dot = read.csv("Data/ClassRoster.csv")


## setting up sub-directories/folders
## you should definitely have a `Data` folder in your project
## use a `Utils` folders is common; it contains handy "utility" functions
source("Utils/randomly_sample_student.R")
randomly_sample_student()
rss()


#### Importing Packages ####
## do this once
# install.packages("lubridate")
## after installing, do this if you want to use a package
library(lubridate)
library("lubridate")

### installing other `tidyverse` packages
library(dyplr)
library(readr)
library(purrr)

## an example when you might want to use a function from the package that does the same thing
## reading with the utils `read.csv()` function
classRosterDf_dot = utils::read.csv("Data/ClassRoster.csv")

## reading with the readr `read_csv()` function
classRosterDf_under = readr::read_csv("Data/ClassRoster.csv")


## notice the outputs to the console
### how nice
classRosterDf_under

### how unruly
classRosterDf_dot

## also notice how it changes the columns names
## `read.cvs()` will change spaces to periods in a column name. 
colnames(classRosterDf_dot) == colnames(classRosterDf_under)
colnames(classRosterDf_dot)
colnames(classRosterDf_under)












