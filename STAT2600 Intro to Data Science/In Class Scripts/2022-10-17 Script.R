library(readr)
library(dplyr)

#### HW 3 ####
collegesDf = read_csv("Homeworks/HW3/colleges.csv")
countiesDf = read_csv("Homeworks/HW3/counties.csv")
maskUseDf = read_csv("Homeworks/HW3/mask_use_by_county.csv")

collSummDf = read_csv("Homeworks/HW3/CollegeSummary.csv")
counSumDf = read_csv("Homeworks/HW3/CountySummary.csv")
nuCollDf = read_csv("Homeworks/HW3/NonUniqueNamedColleges.csv")


#### Least Squares Solution ####
## Fun Fact (i.e. Theorem): The least squares solution of Ax = b is provide by the solution to the system A'Ax = A'y where A' is the transpose of A (i.e. `t(A)`)

## See Gil Strang's first lecture for different ways of matrix multiplication and the "geometry of systems of equations"


#### Visualizing in R ####
## creating scatter plots with plot, points, lines, abline
xVals = 1:3
yVals = c(1, 2, 2)

## let's see
plot(xVals, yVals)

plot(
    xVals, yVals,
    xlim = c(0, 4), ylim = c(0, 3),
    xlab = "X-Axis Label", ylab = "Y-Axis Label",
    main = "Title of Plot",
    cex = .8, pch = 15,
    col = "black"
)

## a reasonable guess for the line of best fit
intercept = 1
slope = .4

## abline plots a line given intercept and slope 
abline(intercept, slope, col = "green")


yPreds = slope*xVals + intercept
## `points` adds points to an existing plot
points(
    xVals, yPreds,
    pch = "*", col = "green",
    cex = 1.5
)

## the best line (as measured by the least square error)
desMat = cbind(1, xVals)

## to solve Ax = b just do `solve(A, b)`
### `t` trasposes matrices; 
#### transpose switch columns with rows
t(desMat)

## why can i just solve it like this?
### need square matrices to use `solve`
solve(desMat, yVals)

### matrix multiplication is done with %*% 
solve(t(desMat) %*% desMat, t(desMat) %*% yVals)


intercept_b = 2/3
slope_b = 1/2
abline(intercept_b, slope_b, col = "blue")
## best solution minimizes sum of squared error

yPreds_b = slope_b*xVals + intercept_b
points(
    xVals, yPreds_b,
    pch = "*", col = "blue",
    cex = 1.5
)

resids_b = yVals - yPreds_b

### numerical evidence for better

resids_b = yVals - yPreds_b
resids = yVals - yPreds
sum(resids^2)
sum(resids_b^2)


#### A Real Life Example ####
houseDf = read_csv(paste0(
    "Data/house-prices-advanced-", 
    "regression-techniques/house_price_train.csv"))


### modeling with square footage 
plot(houseDf$GrLivArea, houseDf$SalePrice, cex = .4)

designMat = cbind(
    Intecept = 1,
    SqFootage = houseDf$GrLivArea
)

coeffVect = solve(t(designMat) %*% designMat, t(designMat) %*% houseDf$SalePrice)

abline(coeffVect[1], coeffVect[2], col = "blue")

## the interpretation of the slope being 107
### property value increases $107 for every square footage