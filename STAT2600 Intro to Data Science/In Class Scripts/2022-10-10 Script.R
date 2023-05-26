#### HW 1 #### 
source("Homeworks/HW1/grade_hw1.R")
source("Utils/grading_helpers.R")

grade_hw1(scriptFilePath = "Homeworks/HW1/hw1.R", returnSum = T, 
          testFileFolder = "Homeworks/HW1/Hw1TestFiles")

myTests = grade_hw1("Homeworks/HW1/hw1.R", returnSum = F,
                    testFileFolder = "Homeworks/HW1/Hw1TestFiles")
myTests[!myTests]

### acronyms and test names




#### Exploratory Data Analysis ####
library(readr)
library(dplyr)
houseDfPath = paste0("Data/house-prices-advanced-regression-techniques", 
                     "/house_price_train.csv")
houseDf = read_csv(houseDfPath)

### columns types 
colTypes = sapply(houseDf, class)

charColNames = colnames(houseDf)[colTypes == "character"]
numColNames = colnames(houseDf)[colTypes == "numeric"]

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

numFeatureNames = numColNames[
    !(numColNames %in% c("MSSubClass", idCol, labelCol, ordinCatFeatureNames))]

catFeatureNames = c("MSSubClass", charColNames[!(charColNames %in% ordinCatFeatureNames)])


#### Summary Statistic #### 

## what columns have missing values? 
has_missing_values <- function(column){
    any(is.na(column))
}

missingValColNames = colnames(houseDf)[sapply(houseDf, has_missing_values)]

percentMissing = (houseDf %>% 
    select(
        one_of(missingValColNames)
    ) %>% 
    is.na() %>% 
    colMeans())*100


### understanding categorical variables
## how can i look at frequency for each category... for all categorical variables 

### doing all frequency tables
catFeatureCountsList = houseDf %>% 
    select(
        one_of(catFeatureNames)
    ) %>% 
    sapply(table) %>% 
    lapply(sort, decreasing = TRUE)

## summarizing numeric variables...
salePriceSummary = summary(houseDf$SalePrice)
numFeatureSummaryList = houseDf %>% 
    select(
        one_of(numFeatureNames)
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
        one_of(numFeatureNames)
    ) %>% 
    lapply(hist)

cust_hist <- function(feature, fName){
    hist(feature,
         breaks = 50,
         freq = FALSE,
         main = paste(fName, "Histogram"),
         xlab = fName)
}

for(iNumFeat in numFeatureNames){
    cust_hist(
        pull(houseDf, iNumFeat),
        fName = iNumFeat)
}

### a better way of visualizing discrete numeric variables

## could use a histogram
hist(houseDf$MoSold,
     breaks = 0:12 + 1/2)

## but better is to use a barplot
barplot(table(houseDf$MoSold),
        main = "Barplot of Month Sold",
        xlab = "Months",
        ylab = "Counts",
        # density = 50,
        angle = 0,
        col = "dodgerblue4")


