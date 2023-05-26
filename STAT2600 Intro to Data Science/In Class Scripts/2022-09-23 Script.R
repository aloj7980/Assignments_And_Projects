#####
## HW 2 Questions

#### Manipulating Columns ####
## use `select` and `mutate` to subset and create new columns
library(readr)
library(dplyr)

eplDf = read_csv("Data/epl2122.csv")
eplDf %>% View()

## new column for total number of goals
eplDf %>% 
    mutate(
        TotalGoals = FTHG + FTAG, 
        TotalShots = HS + AS
    ) %>% 
    select(
        FTHG, 
        FTAG,
        TotalGoals,
        TotalShots
    )

## to do without dplyr function
eplDf[, "TotalGoals"] = eplDf[, "FTHG"] + eplDf[, "FTAG"]


## new columns for winning team name
eplDf %>% 
    mutate(
        WinningTeams = ifelse(
            FTR == "H", 
            HomeTeam,
            ifelse(
                FTR == "A", 
                AwayTeam, 
                "Draw"))
    ) %>% 
    select(
        HomeTeam,
        AwayTeam,
        FTR,
        WinningTeams
    ) %>% 
    filter(
        FTR == "D"
    )

## new column for number of points for the home/away team; 3pts for win, 1pt for draw, 0 for loss
eplDf %>% 
    mutate(
        HomePoints = ifelse(
            FTR == "H", 3, ifelse(FTR == "D", 1, 0)),
        AwayPoints = ifelse(
            FTR == "A", 3, ifelse(FTR == "D", 1, 0)),
    ) %>% 
    select(
        FTR,
        HomePoints,
        AwayPoints
    ) %>% 
    filter(
        FTR == "D"
    )

#### Manipulating Rows ####
## use filter and arrange to subset and sort rows

## filter the rows to just home games for Arsenal
eplDf %>% 
    filter(
        HomeTeam == "Arsenal"
    )

## filter to rows where the combined number of goals is greater than 4? 
eplDf %>% 
    mutate(
        TotalGoals = FTHG + FTAG
    ) %>% 
    filter(
        TotalGoals > 4
    )

## filter to rows where their is a comback? (i.e. losing team at half time is winning team at full time)
eplDf %>% 
    filter(
        (FTR != HTR) & (FTR != "D") & (HTR != "D") 
    ) %>% 
    select(
        FTR,
        HTR
    )


#### Aggregating Dataframes ####
## use `group_by` and `summarize`

## what are the total and average number of home goals?
eplDf %>% 
    summarise(
        TotalHomeGoals = sum(FTHG),
        MeanHomeGoals = mean(FTHG),
        MaxHomeGoals = max(FTHG)
    )

## what happens when i use aggregating function in  `mutate`

eplDf %>% 
    mutate(
        TotalHomeGoals = sum(FTHG)
    ) %>% 
    select(
        FTHG,
        TotalHomeGoals
    )

## how many games were won by the home team? 
eplDf %>% 
    mutate(
        HomeTeamWins = (FTR == "H"),
        AwayTeamWins = FTHG < FTAG
    ) %>% 
    summarize(
        NHomeWins = sum(HomeTeamWins),
        NAwayWins = sum(AwayTeamWins)
    )

eplDf %>% 
    filter(
        FTR == "H"
    )


#### Final League Table ####
## can we recreate the final table?? 
### colnames(finalTable) = c("Team", "MatchesPlayed", "Wins", "Draws", "Losses", "GoalsFor", "GoalsAgainst", "GoalDifference", "Points")

## an aggregation of all the home results
homeTeamDf = eplDf %>% 
    group_by(
        HomeTeam
    ) %>% 
    summarise(
        GamesPlayed = n(),
        HomeWin = sum(FTR == "H"),
        HomeDraw = sum(FTR == "D"),
        HomeLoss = sum(FTR == "A"),
        HomeGoalsFor = sum(FTHG),
        HomeGoalsAgainst = sum(FTAG)
    ) %>% 
    mutate(
        HomeGoalDifference = HomeGoalsFor -
            HomeGoalsAgainst,
        Points = HomeWin * 3 + HomeDraw
    ) %>% 
    arrange(
        desc(Points),
        desc(HomeGoalDifference)
    )
homeTeamDf %>% View()

### To be continued





### the fact we needed to split this data into home and away should tell you this data isn't actually tidy... could we organize the data in another way to do this without the intermediate tables?? 

#### Bonus ####
## is Mike Dean the most prolific referee (i.e. gives out the most cards)  in the EPL


#### Appendix: Data Dictionary ####

# "Date"
# "HomeTeam"
# "AwayTeam"
# "FTHG": FullTimeHomeGoals
# "FTAG": FullTimeAwayGoals
# "FTR": FullTimeResult
# "HTHG": HalfTimeHomeGoals
# "HTAG": HalfTimeAwayGoals
# "HTR": HalfTimeResult
# "Referee"
# "HS": HomeShots
# "AS": AwayShots
# "HST": HomeShotsOnTarget
# "AST": AwayShotsOnTarge
# "HF": HomeFouls
# "AF": AwayFould
# "HC": HomeCorners
# "AC": AwayCorners
# "HY": HomeYellow
# "AY": AwayYellow
# "HR": HomeRed
# "AR": AwayRed