library(readr)
library(dplyr)

#### Exploratory Data Analysis ####
houseDfPath = paste0("Data/house-prices-advanced-regression-techniques", 
                     "/house_price_train.csv")
houseDf = read_csv(houseDfPath)
houseDf
## Q: what is the goal of an exploratory data analysis
## A: 
### 1. what is the problem at hand; what do i want to do/find with this dataset. 
### 2. what withing the dataset will help me better answer/address the problem at hand.
### 3. size of dataset
### 4. understand what the rows/record are
### 5. understand what the columns (and their types) of the dataset are
### 6. do any of the columns need preprocessed/transformed/cleaned
### 7. looking for outliers/missing values
### 8. summary statistics for columns

### all the time. initial observations/questions


#### Different Data Type ####
## what is the size of the dataset?
### 1460 rows and 81 columns
dim(houseDf)

## what are the data types for the different columns?
sapply(houseDf, class) %>% unique()
sapply(houseDf, class) %>% table()

colTypes = sapply(houseDf, class)

charColNames = colnames(houseDf)[colTypes == "character"]
numColNames = colnames(houseDf)[colTypes == "numeric"]

## looking into each variable 
#### Observation #### 
# columns that are not numeric:
## MSSubClass


## what columns have missing values? 
has_missing_values <- function(column){
    any(is.na(column))
}
missingValColNames = colnames(houseDf)[sapply(houseDf, has_missing_values)]


#### summary statistic #### 


### EDA visuals 