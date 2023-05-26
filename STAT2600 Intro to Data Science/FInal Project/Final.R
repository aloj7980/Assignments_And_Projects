require(stringr)
require(readr)
require(dplyr)
require(data.tree)
statsDf <- read_csv("Seasons_Stats.csv")
salariesDf <- read_csv("salaries.csv")
averagesDf <- read_csv("leagueAverages.csv")

## inner join data frames together
newDf <- statsDf %>% inner_join(salariesDf,by=c("Player" = "Player Name", "Year" = "Season End","Tm" = "Team"))
newDf <- newDf[!is.na(newDf$Player),]

newDf <- subset(newDf, select=c("Year","Player","Age","Tm","G","MP","PER","TS%","3PAr","FTr","ORB%","DRB%","TRB%","AST%","STL%","BLK%","TOV%","USG%","OWS","DWS","WS","WS/48","OBPM","DBPM","BPM","VORP","FG%","3P%","2P%","eFG%","FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","Salary in $"))
colnames(newDf)[41] ="salary"

##Filter out records with <=100 minutes
newDf <- newDf %>% filter(MP > 200)

## adjust salaries to be a percentage of nba salary cap
cap = c(11871000,12500000,14000000,15175000,15964000,23000000,
           24693000,26900000,30000000,34000000,35500000,42500000,
           40271000,43840000,43870000,49500000,53135000,55630000,
           58680000,57700000,58040000,58044000,58044000,58679000,
           63065000,70000000,94143000)

newDf <- newDf %>% mutate(salary = gsub(',','',salary)) %>%
  mutate(salary = as.numeric(sapply(salary,str_extract,"[0-9]+"))) %>%
  mutate(cap_percentage = salary/cap[Year-1990])

## adjust applicable stats for era
adjustableStats = c("TS%","FG%","3P%","2P%","eFG%","FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")
leagueTS = c(.534,.531,.536,.528,.543,.542,.536,.524,.511,.523,.518,.52,.519,.516,.529,.536,.541,.54,.544,.543,.541,.527,.535,.541,.534,.541,.552,.556,.56,.565,.572,.566,.574)
league2P = c(.488,.486,.489,.483,.491,.486,.48,.47,.457,.468,.461,.465,.463,.46,.47,.478,.485,.484,.485,.492,.487,.477,.483,.488,.485,.491,.503,.51,.52,.524,.53,.533,.54)
newDf <- newDf %>%
  mutate("TS%" = newDf$"TS%"-leagueTS[Year-1990]) %>%
  mutate("2P%" = newDf$"2P%"-league2P[Year-1990]) %>%
  mutate("FG%" = newDf$"FG%"-averagesDf$`FG%`[Year-1990]) %>%
  mutate("FT%" = newDf$"FT%"-averagesDf$`FT%`[Year-1990]) %>%
  mutate("3P%" = newDf$"3P%"-averagesDf$`3P%`[Year-1990]) %>%
  mutate("eFG%" = newDf$"eFG%"-averagesDf$`eFG%`[Year-1990]) %>%
  mutate("ORB" = newDf$"ORB"/averagesDf$`ORB`[Year-1990]) %>%
  mutate("DRB" = newDf$"DRB"/averagesDf$`DRB`[Year-1990]) %>%
  mutate("TRB" = newDf$"TRB"/averagesDf$`TRB`[Year-1990]) %>%
  mutate("AST" = newDf$"AST"/averagesDf$`AST`[Year-1990]) %>%
  mutate("STL" = newDf$"STL"/averagesDf$`STL`[Year-1990]) %>%
  mutate("BLK" = newDf$"BLK"/averagesDf$`BLK`[Year-1990]) %>%
  mutate("TOV" = newDf$"TOV"/averagesDf$`TOV`[Year-1990]) %>%
  mutate("PF" = newDf$"PF"/averagesDf$`PF`[Year-1990]) %>%
  mutate("PTS" = newDf$"PTS"/averagesDf$`PTS`[Year-1990])

View(newDf)

##Functions to build decision tree
percentileStats = c("PER","TS%","3PAr","FTr","ORB%","DRB%","TRB%","AST%","STL%",
                    "BLK%","TOV%","USG%","OWS","DWS","WS","WS/48","OBPM","DBPM",
                    "BPM","VORP","FG%","3P%","2P%","eFG%","FT%","ORB","DRB","TRB",
                    "AST","STL","BLK","TOV","PF","PTS")

calculate_percentiles <- function(newDf){
  newDf <- newDf %>%
  mutate("PER" = rank(newDf$PER)/length(newDf$PER)) %>%
  mutate("TS%" = rank(newDf$`TS%`)/length(newDf$`TS%`)) %>%
  mutate("3PAr" = rank(newDf$`3PAr`)/length(newDf$`3PAr`)) %>%
  mutate("FTr" = rank(newDf$FTr)/length(newDf$FTr)) %>%
  mutate("ORB%" = rank(newDf$`ORB%`)/length(newDf$`ORB%`)) %>%
  mutate("DRB%" = rank(newDf$`DRB%`)/length(newDf$`DRB%`)) %>%
  mutate("TRB%" = rank(newDf$`TRB%`)/length(newDf$`TRB%`)) %>%
  mutate("AST%" = rank(newDf$`AST%`)/length(newDf$`AST%`)) %>%
  mutate("STL%" = rank(newDf$`STL%`)/length(newDf$`STL%`)) %>%
  mutate("BLK%" = rank(newDf$`BLK%`)/length(newDf$`BLK%`)) %>%
  mutate("TOV%" = rank(newDf$`TOV%`)/length(newDf$`TOV%`)) %>%
  mutate("USG%" = rank(newDf$`USG%`)/length(newDf$`USG%`)) %>%
  mutate("OWS" = rank(newDf$OWS)/length(newDf$OWS)) %>%
  mutate("DWS" = rank(newDf$DWS)/length(newDf$DWS)) %>%
  mutate("WS" = rank(newDf$WS)/length(newDf$WS)) %>%
  mutate("WS/48" = rank(newDf$`WS/48`)/length(newDf$`WS/48`)) %>%
  mutate("OBPM" = rank(newDf$OBPM)/length(newDf$OBPM)) %>%
  mutate("DBPM" = rank(newDf$DBPM)/length(newDf$DBPM)) %>%
  mutate("BPM" = rank(newDf$BPM)/length(newDf$BPM)) %>%
  mutate("VORP" = rank(newDf$VORP)/length(newDf$VORP)) %>%
  mutate("FG%" = rank(newDf$`FG%`)/length(newDf$`FG%`)) %>%
  mutate("2P%" = rank(newDf$`2P%`)/length(newDf$`2P%`)) %>%
  mutate("3P%" = rank(newDf$`3P%`)/length(newDf$`3P%`)) %>%
  mutate("eFG%" = rank(newDf$`eFG%`)/length(newDf$`eFG%`)) %>%
  mutate("FT%" = rank(newDf$`FT%`)/length(newDf$`FT%`)) %>%
  mutate("ORB" = rank(newDf$ORB)/length(newDf$ORB)) %>%
  mutate("DRB" = rank(newDf$DRB)/length(newDf$DRB)) %>%
  mutate("TRB" = rank(newDf$TRB)/length(newDf$TRB)) %>%
  mutate("AST" = rank(newDf$AST)/length(newDf$AST)) %>%
  mutate("STL" = rank(newDf$STL)/length(newDf$STL)) %>%
  mutate("BLK" = rank(newDf$BLK)/length(newDf$BLK)) %>%
  mutate("TOV" = rank(newDf$TOV)/length(newDf$TOV)) %>%
  mutate("PF" = rank(newDf$PF)/length(newDf$PF)) %>%
  mutate("PTS" = rank(newDf$PTS)/length(newDf$PTS))
}

find_max_split <- function(d){
  if("list" %in% class(d)){
    d = as.data.frame(do.call(rbind, d)) 
  }
  lower = c()
  upper = c()
  max_diff = 0
  max_diff_lower_df = d
  max_diff_upper_df = d
  for(i in 2:ncol(d)-1){
    lowerDf = d %>% filter(d[i] <= 0.49999999)
    upperDf = d %>% filter(d[i] >= 0.5)
    lower = append(lower,mean(lowerDf$cap_percentage))
    upper = append(upper,mean(upperDf$cap_percentage))
    if(abs(mean(upperDf$cap_percentage)-mean(lowerDf$cap_percentage)) > max_diff){
      max_diff_lower_df = lowerDf
      max_diff_upper_df = upperDf
      max_diff = abs(mean(upperDf$cap_percentage)-mean(lowerDf$cap_percentage))
    }
  }
  diffs = abs(upper - lower)
  myIndex = which(diffs == max(diffs))
  max_diff_lower_df[myIndex] = max_diff_lower_df[myIndex]*2
  max_diff_upper_df[myIndex] = (max_diff_upper_df[myIndex]-0.5)*2
  max_diff_lower_df = calculate_percentiles(max_diff_lower_df)
  max_diff_upper_df = calculate_percentiles(max_diff_upper_df)
  return(list(colnames(d)[myIndex],lower[myIndex],upper[myIndex],max_diff_lower_df,max_diff_upper_df))
}

build_decision_tree <- function(d,x){
  result = find_max_split(d)
  if(x > 0){
    print(c(result[1],result[2],result[3],x))
    build_decision_tree(result[4],x-1)
    build_decision_tree(result[5],x-1)
  }
}

allStats=newDf[c(7:40,42)]
allStats = calculate_percentiles(allStats)

# This call to build_decision_tree() will recursively print out each node in the tree
# with the stat it's split by, the average cap percentage of the values below and above
# or equal to the 50th percentile, and the level of the tree it's on, with 5 being the 
# root. These printed values were used to build the tree pictured in the report.
build_decision_tree(allStats,5)


# ctree() model
allStats2=newDf[c(7:40,42)]
require(party)
require(Metrics)
sample <- sample(c(TRUE, FALSE), nrow(allStats2), replace=TRUE, prob=c(0.8,0.2))
train  <- allStats2[sample, ]
test   <- allStats2[!sample, ]
tree = ctree(cap_percentage ~ .,data=allStats2)
plot(tree)
#RMSE of training set
p <- predict(tree, train)
RMSE = rmse(train$cap_percentage, p)
print(RMSE)
#RMSE of test set
pTest <- predict(tree, test)
RMSETest = rmse(test$cap_percentage, pTest)
print(RMSETest)

# Linear regression
myLM = lm(cap_percentage ~ `USG%` + VORP + TRB + STL + TOV + `AST%` + `TOV%` , data = train)
summary(myLM)
#RMSE of training set
pLM <- predict(myLM, train)
RMSELM = rmse(train$cap_percentage, pLM)
print(RMSELM)
#RMSE of test set
pLMTest <- predict(myLM, test)
RMSELMTest = rmse(test$cap_percentage, pLMTest)
print(RMSELMTest)

# Logistic Regression
logModel = glm(formula = cap_percentage ~ PTS + PER + TRB, data = train, family = "binomial")
summary(logModel)
#Log Loss of training set
log_loss <- function(yTrue, yPred){
  -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))
}
LogLoss = log_loss(yTrue = train$cap_percentage, yPred = logModel$fitted.values)
print(LogLoss)
#RMSE of test set
pTestLog <- predict(logModel, test)
pTestLogVals <- exp(pTestLog) / (1 + exp(pTestLog))
LogLossTest = log_loss(yTrue = test$cap_percentage, yPred = pTestLogVals)
print(LogLossTest)





