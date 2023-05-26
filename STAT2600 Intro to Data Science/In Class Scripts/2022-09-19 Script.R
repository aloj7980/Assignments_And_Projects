#### HW 1 Extra ####

## Question: what are the most popular functions that I use in class? 

library(readr)
source("Homeworks/HW1/BonusMaterial/hw1_extra.R")
source("Utils/helper_functions.R")

lectureScriptPaths = paste(
    "LectureScripts",
    list_files_no_icon("LectureScripts"),
    sep = "/")

functionsUsedInLecture = character()

for(iScriptPath in lectureScriptPaths){
    iFunctions = iScriptPath %>% 
        read_text_file(
            withoutBlankLines = TRUE, 
            withoutCommentedLines = TRUE) %>% 
        extract_functions_from_lines()
    functionsUsedInLecture = c(functionsUsedInLecture, iFunctions)
}

mostPopularFunctionsDF = functionsUsedInLecture %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    as.data.frame()

## the unpiped version of the above code
as.data.frame(sort(table(functionsUsedInLecture), decreasing = TRUE)) 


colnames(mostPopularFunctionsDF) = c("FunctionName", "Frequency")
mostPopularFunctionsDF

mostPopularFunctionsDF %>% write_csv(
    "Homeworks/HW1/BonusMaterial/MostPopularFunctions.csv")


#### Messy Data ####
## "Happy families are all alike; every unhappy family is unhappy in it's own way." -Leo Tolstoy

## where does bad/messy data come from?
### Demo: an informal survey... 


## what does a particular instance of messy data look like?

dataScienceJobDfPath = "Data/ds_salaries2.csv"
read.csv(dataScienceJobDfPath)
read_csv(dataScienceJobDfPath)
dsJobDf = read_csv(dataScienceJobDfPath)
dsJobDf %>% View()

dsJobDf %>% colnames()
## the upiped version
colnames(dsJobDf)

## we are subsetting with the dollar sign ($)
### if a column has a space in its name, need to surround it in backticks (``)
dsJobDf$`Job Title`
dsJobDf$`Job Title` %>% unique() %>% length()

## this columns isnt useful; wish it were two numerics
dsJobDf$`Salary Estimate`


dsJobDf$Location


#### Tibble vs Dataframes ####

## using base::read.csv
read.csv(dataScienceJobDfPath)

## using readr:read_csv
read_csv(dataScienceJobDfPath)

## using tibbles. 
library(tibble)
## read/play with this for homework
vignette("tibble")


#### Cleaning Columns ####


## creating new columns
## let's split the city and the state

dsJobDf[, "City"] <- str_extract(dsJobDf$`Location`, "[[:alpha:][:blank:]]+")

## subsetting like this returns a tibble
dsJobDf[, "City"]
class(dsJobDf[, "City"])

## subsetting like this returns a character vector
dsJobDf$City
class(dsJobDf$City)

library(dplyr)
pull(dsJobDf, City)
dsJobDf %>% 
    pull(City)

## can assing a columns with `mutate()` as well

dsJobDf = mutate(
    dsJobDf, 
    State = str_sub(dsJobDf$`Location`, start = -2, end = -1))

dsJobDf %>% 
    mutate(
        State = str_sub(dsJobDf$`Location`, start = -2, end = -1),
        City = str_extract(dsJobDf$`Location`, "[[:alpha:][:blank:]]+")
    )
