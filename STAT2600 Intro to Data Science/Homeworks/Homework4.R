#data
head(mtcars, 10)

#basic tests
summary(lm(mpg ~ wt + qsec, data = mtcars))
c(
  RMSE = sqrt(mean(resid(lm(mpg ~ wt + qsec, data = mtcars))^2)),
  RSq = summary(lm(mpg ~ wt + qsec, data = mtcars))$r.squared
)

#advanced tests
transMtcarsDf = mtcars %>%
  transmute(
    mpg = mpg,
    wt = wt,
    qsec = qsec
  )
head(transMtcarsDf, 5)

rmse <- function(yTrue, yPred){
  sqrt(mean((yTrue - yPred)^2))
}
r_squared <- function(yTrue, yPred){
  ssr = sum((yTrue - yPred)^2)
  yBar = mean(yTrue)
  tss = sum((yTrue - yBar)^2)
  1 - (ssr/tss)
}
train_test_split <- function(dataset, propTrain = .8, seed = NULL){
  if(length(seed)){
    set.seed(seed)
  }
  nrowTrain = nrow(dataset) * propTrain
  allInds = 1:nrow(dataset)
  trainingInds = sample(allInds, size = nrowTrain)
  testInds = (allInds)[!(allInds %in% trainingInds)]
  return(
    list(
      train = dataset[trainingInds, ],
      test = dataset[testInds, ]
    )
  )
}
nSims = 10^3
rmseVals = numeric(nSims)
rSqVals = numeric(nSims)
for(iSim in 1:nSims){
  iTrainTestSplit = train_test_split(transMtcarsDf, seed = iSim)
  iTrain = iTrainTestSplit$train
  iTest = iTrainTestSplit$test
  iLm = lm(mpg ~ ., data = iTrain)
  iPreds_test = predict(iLm, data.frame(iTest %>% select(-mpg)))
  rmseVals[iSim] = rmse(iTest$mpg, iPreds_test)
  rSqVals[iSim] = r_squared(iTest$mpg, iPreds_test)
}
c(
  MeanRmse = mean(rmseVals),
  MeanRSq = mean(rSqVals)
)

#write csvs
modelLm = lm(mpg ~ wt + qsec, data = transMtcarsDf)
mtcarsDf = cbind.data.frame(
  CarName = rownames(transMtcarsDf),
  transMtcarsDf,
  FittedValue = modelLm$fitted.values,
  Residual = modelLm$residuals
)
rownames(mtcarsDf) = NULL
mtcarsDf
write_csv(mtcarsDf, "MtcarsDf.csv")

featureSummary = cbind.data.frame(
  Feature = c("Intercept", colnames(transMtcarsDf)[-1]),
  Coefficient = modelLm$coefficients,
  PValue = summary(modelLm)$coefficients[, 4]
)
rownames(featureSummary) = NULL
featureSummary
write_csv(featureSummary, "FeatureSummary.csv")