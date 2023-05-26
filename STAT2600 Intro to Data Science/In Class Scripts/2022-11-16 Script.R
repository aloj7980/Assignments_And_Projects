#### Lab 13 Questions ####
library(readr)
library(dplyr)

sampleMatrix = read_csv("Labs/Lab13_20221115/sample_data_lab13.csv")


#### Logistic Regression ####

## Admissions Data
### `admit` (target/label): whether or not individual was admitted
### `gre`: gre score; between 200-800
### `gpa`: grade point average; between 0-4
### `rank`: college rank; between 1-4, 1 most prestigious, 4 least prestigious

admitDf = read_csv("Data/AdmissionData.csv")
admitDf 
admitDf %>% summary()

(admitDf$admit %>% mean()) * 100
admitDf$gre %>% hist(breaks = 30)
admitDf$gpa %>% hist(breaks = 30)
admitDf$rank %>% table() 
admitDf$rank %>% table() %>% barplot()

## Mapping R into [0, 1]
logit <- function(x){1/(1 + exp(-x))}
plot((-100:100)/25, logit((-100:100)/25), type = "l", xlab = "Real Number Space", ylab = "Probability Space")
abline(0, 0)
lines(c(0, 0), 0:1)



## Modeling
logMod_wGre = glm(admit ~ gre , data = admitDf, family = "binomial")
logMod_wGre
exp(- 3 + 200*0.003582)
1/(1+exp(3 - 200*0.003582))
logMod_wGre %>% summary()


logMod_wGreGpa = glm(admit ~ gre + gpa, data = admitDf, family = "binomial")
logMod_wGreGpa
exp(- 3 + 200*0.003582)
1/(1+exp(3 - 200*0.003582))
logMod_wGre %>% summary()

glm(admit ~ gre, data = admitDf, family = "binomial") %>% summary()
log_loss <- function(yTrue, yPred){
    -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))
}

