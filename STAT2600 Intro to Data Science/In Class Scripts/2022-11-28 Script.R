#### Logistic Regression ####

## Admissions Data
### `admit` (target/label): whether or not individual was admitted
### `gre`: gre score; between 200-800
### `gpa`: grade point average; between 0-4
### `rank`: college rank; between 1-4, 1 most prestigious, 4 least prestigious
library(readr)
library(dplyr)

admitDf = read_csv("Data/AdmissionData.csv")
admitDf 
admitDf %>% summary()

(admitDf$admit %>% mean()) * 100
admitDf$gre %>% hist(breaks = 30, main = "GRE Scores")
admitDf$gpa %>% hist(breaks = 30, main = "GPA")
admitDf$rank %>% table() %>% barplot(main = "Undergrad Rank")

#### Mapping R into [0, 1] ####
logit <- function(x){1/(1 + exp(-x))}
plot((-100:100)/25, logit((-100:100)/25), type = "l", xlab = "Real Number Space", ylab = "Probability Space")
abline(0, 0)
lines(c(0, 0), 0:1)



#### Modeling ####
logMod_wGre = glm(admit ~ gre, data = admitDf, family = "binomial")
logMod_wGre %>% summary()

logMod_wGpa = glm(admit ~ gpa, data = admitDf, family = "binomial")
logMod_wGpa %>% summary()

logMod_wGreGpa = glm(admit ~ gre + gpa, data = admitDf, family = "binomial")
logMod_wGreGpa %>% summary()

logMod_wGreGpa$coefficients
logMod_wGreGpa$fitted.values

# which individual has the highest probability
which.max(logMod_wGreGpa$fitted.values)
logMod_wGreGpa$fitted.values
admitDf[3, ]


#### Loss Function: Log Loss ####
## how are coefficients determined? in normal linear regression we minimize Root Mean Squared Error (RMSE). in logistic regression we minimize log loss!

## log loss is just a specific instance of the so-called cross-entropy between the data and the predictions

log_loss <- function(yTrue, yPred){
    -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))
}

log_loss(yTrue = admitDf$admit, yPred = logMod_wGreGpa$fitted.values)


predVals = logit(cbind(1, admitDf$gre, admitDf$gpa) %*% logMod_wGreGpa$coefficients)

predVals == logMod_wGreGpa$fitted.values

predVals - logMod_wGreGpa$fitted.values
max(predVals - logMod_wGreGpa$fitted.values)

all(dplyr::near(predVals, logMod_wGreGpa$fitted.values))


predVals_perturbed = logit(
    cbind(1, admitDf$gre, admitDf$gpa) %*% (
        logMod_wGreGpa$coefficients + rnorm(3, sd = 0.001)))

c(
    log_loss(yTrue = admitDf$admit, yPred = logMod_wGreGpa$fitted.values),
    log_loss(yTrue = admitDf$admit, yPred = predVals_perturbed)
)


