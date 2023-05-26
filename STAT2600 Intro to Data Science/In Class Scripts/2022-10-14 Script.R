#### Linear Algebra ####

## Distances and Norms

## Line of Best Fit

## Systems of Equations
### The Design Matrix

## Solving Systems of Equations


#### Visualizing in R ####

xVals = 1:3
yVals = c(1, 2, 2)

## `plot()` can be used to plot points
plot(
    xVals, yVals,
    xlim = c(0, 4), ylim = c(0, 3)
)

intercept = 1
slope = .4
## abline can be used to plot a line in slope-intercept form
## `col` will change the color of the plot
abline(intercept, slope, col = "green")

## the point on the line
yPreds = slope*xVals + intercept

## `points` will add points to an existing plot
points(
    xVals, yPreds,
    pch = "*", col = "green", cex =2
)

## yVals = yPreds + resids
resids = yVals - yPreds

## `lines` will add lines to an existing plot 
lines(
    c(xVals[1], xVals[1]), 
    c(yVals[1], yPreds[1]),
    col = "red"
)

for(iPt in 1:length(xVals)){
    lines(
        c(xVals[iPt], xVals[iPt]), 
        c(yVals[iPt], yPreds[iPt]),
        col = "red"
    )
}

## the sum of the residuals squared
sqrt(sum(resids^2))


#### Least Squares Solution ####
## Fun Fact (i.e. Theorem): The least squares solution of Ax = b is provide by the solution to the system A'Ax = A'y where A' is the transpose of A (i.e. `t(A)`)

## creating the design matrix
X = cbind(1, xVals)
coeffs_b = solve(t(X) %*% X, t(X) %*% yVals)
coeffs_b

## let's see if these look better 
plot(
    xVals, yVals,
    xlim = c(0, 4), ylim = c(0, 3)
)
intercept = 1
slope = .4
abline(intercept, slope, col = "green")


yPreds = slope*xVals + intercept
points(
    xVals, yPreds,
    pch = "*", col = "green",
    cex = 2
)

## new "best"? model
intercept_b = coeffs_b[1]
slope_b = coeffs_b[2]
abline(intercept_b, slope_b, col = "blue")


## looks reasonable 
yPreds_b = slope_b*xVals + intercept_b
points(
    xVals, yPreds_b,
    pch = "*", col = "blue",
    cex = 2
)

## what above the norm of the residuals
resids_b = yVals - yPreds_b

### numerical evidence for better
sqrt(sum(resids^2))
sqrt(sum(resids_b^2))

