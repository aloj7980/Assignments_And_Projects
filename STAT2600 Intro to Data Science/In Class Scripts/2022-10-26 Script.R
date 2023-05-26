#### Lab 10 ####
## all the different types of sorting

#### HW2 Grading ####
hw2Path = "Homeworks/HW2"
studentDfNames = list.files(hw2Path) %>% sort()
studentDfPaths= paste(hw2Path, studentDfNames, sep = "/")


iStudent = "Sample"
iDfPaths = str_subset(studentDfPaths, iStudent)

personDf = read_csv("Homeworks/HW2/Sample_PersonTable.csv")
showDf = try_catch_null(read_csv_quiet(str_subset(iDfPaths, "ShowTable-*[:digit:]*.csv"))) %>% 
    rename(
        ReleaseYear = RelaseYear
    )
movieDf = try_catch_null(read_csv_quiet(str_subset(iDfPaths, "MovieTable-*[:digit:]*.csv")))

source("Homeworks/HW2/grade_hw2.R")
myGrades = grade_hw2(personDf, showDf, movieDf)
sum(myGrades)
myGrades[!myGrades]


#### Non-linear Linear Models? ####
## AKA Transforming Models 

## height (cm) and weight (kg)

htWtDf = read_csv("Data/HeightWeight.csv")

plot(htWtDf$height, htWtDf$weight)
linModel = lm(weight ~ height)
class(linModel)
abline(linModel$coefficients[1], linModel$coefficients[2], col = "blue")

## residuals are difference between predictions and true values
plot(height, linModel$residuals)
abline(0,0, col = "blue")

## the residuals tell us that this model isn't quite right, because they aren't white noise (i.e they display a distinct non-linear pattern)

