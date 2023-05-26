
#### Creating League Table (cont) ####
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

eplDf_wide = read_csv("EplGames_2122Season.csv")

eplDf <- eplDf_wide %>% select(Date:FTR) %>% pivot_longer(HomeTeam:AwayTeam) %>% 
  transmute(
    Date = Date %>% dmy() %>% ymd(),
    Team = value,
    HomeAway = str_sub(name,1,4),
    GoalsFor = ifelse(HomeAway == "Home", FTHG, FTAG),
    GoalsAgainst = ifelse(HomeAway == "Home", FTAG, FTHG),
    Result = ifelse(GoalsFor == GoalsAgainst, "Draw", ifelse(GoalsFor > GoalsAgainst, "Win", "Loss"))
  )
eplDf %>% write_csv("lab6.csv")