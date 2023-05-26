#### HW 3 #### 
library(readr)
library(dplyr)


collegesDf = read_csv("Homeworks/HW3/colleges.csv") 
countiesDf = read_csv("Homeworks/HW3/counties.csv")
maskUseDf = read_csv("Homeworks/HW3/mask_use_by_county.csv")

collegesDf %>% 
    filter(
        college == "Southwestern College"
    ) %>% 
    select(
        state:city,
        college
    )

countiesDf

#### Exploratory Data Analysis ####

houseDfPath = paste0("Data/house-prices-advanced-regression-techniques", 
                     "/house_price_train.csv")
houseDf = read_csv(houseDfPath)
houseDf
houseDf %>% View()

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

### all the time: observations/questions


#### Different Data Type ####
## numeric: 
### continuous
### discrete
## categorical:
### ordinal (i.e. is there a meaningful order)
### non-ordinal 

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
## columns that are not in fact numeric:
### MSSubClass
class(houseDf$MSSubClass)

## columns that are ordinal categorical:
ordinCatFeatureNames = c(
    "Utilities", "LandSlope", "OverallQual", "OverallCond", 
    "ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "BsmtExposure",
    "BsmtFinType1", "BsmtFinType2", "HeatingQC", "KitchenQual", 
    "Functional", "FireplaceQu", "GarageFinish", "GarageQual", 
    "PoolQC", "Fence" 
)

## column names by type

idCol = "Id"
labelCol = "SalePrice"
# numericCols
    
numFeatureNames = numColNames[
    !(numColNames %in% c("MSSubClass", idCol, labelCol, ordinCatFeatureNames))]

catFeatureNames = c("MSSubClass", charColNames[!(charColNames %in% ordinCatFeatureNames)])

ordinCatFeatureNames

## checking to see that all columns are accounted for
dim(houseDf)[2] == 1 + 1 + length(numFeatureNames) + 
    length(catFeatureNames) + length(ordinCatFeatureNames)


#### Summary Statistic #### 

## what columns have missing values? 
has_missing_values <- function(column){
    any(is.na(column))
}

missingValColNames = colnames(houseDf)[sapply(houseDf, has_missing_values)]

percentMissing = (houseDf %>% 
    select(
        missingValColNames
    ) %>% 
    is.na() %>% 
    colMeans())*100

## another way to calculate percent missing with apply
houseDf %>% 
        select(
            missingValColNames
        ) %>% 
        is.na() %>% 
    apply(MARGIN = 2, mean)


### understanding categorical variables
## how can i look at frequence for each category for all categorical variables 

## a single frequency table
table(houseDf$Neighborhood) %>% sort(decreasing = TRUE)

## what's up with the "NAmes"? Northern AMES


### doing all frequency tables
catFeatureCountsList = houseDf %>% 
    select(
        catFeatureNames
    ) %>% 
    sapply(table) %>% 
    lapply(sort, decreasing = TRUE)

## might do similar thing for ordinal categorical variables

## summarizing numeric variables...

summary(houseDf$SalePrice)


numFeatureSummaryList = houseDf %>% 
    select(
        numFeatureNames
    ) %>% 
    lapply(summary)


#### EDA Visuals #### 
### how do i visualize numeric data? 

hist(houseDf$SalePrice,
     breaks = 50,
     freq = FALSE,
     main = "Price",
     xlab = "Sale Price")



houseDf %>% 
    select(
        numFeatureNames
    ) %>% 
    lapply(hist)



houseDf[order(houseDf$SalePrice[is.na(houseDf$LotFrontage)]), ]$SalePrice

bind_rows(
    houseDf %>% 
        filter(
            !is.na(LotFrontage)) %>% 
        arrange(
            desc(SalePrice)),
    houseDf %>% 
        filter(
            is.na(LotFrontage)) %>% 
        arrange(
            desc(SalePrice))
) %>% View()

### how do i visualize categorical data?

library(readxl)

writeex
