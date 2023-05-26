
#### Creating League Table (cont) ####
library(readr)
library(dplyr)
library(tidyr)

eplDf = read_csv("Data/epl2122.csv")
eplDf %>% View()

## an aggregation of all the home results
homeTeamDf = eplDf %>% 
    group_by(
        HomeTeam
    ) %>% 
    summarise(
        GamesPlayed_home = n(),
        HomeWin = sum(FTR == "H"),
        HomeDraw = sum(FTR == "D"),
        HomeLoss = sum(FTR == "A"),
        HomeGoalsFor = sum(FTHG),
        HomeGoalsAgainst = sum(FTAG)
    ) %>% 
    mutate(
        HomeGoalDifference = HomeGoalsFor -
            HomeGoalsAgainst,
        Points_home = HomeWin * 3 + HomeDraw
    ) %>% 
    arrange(
        desc(Points_home),
        desc(HomeGoalDifference)
    )

homeTeamDf %>% View()


awayTeamDf = eplDf %>% 
    group_by(
        AwayTeam
    ) %>% 
    summarise(
        GamesPlayed_away = n(),
        AwayWin = sum(FTR == "A"),
        AwayDraw = sum(FTR == "D"),
        AwayLoss = sum(FTR == "H"),
        AwayGoalsFor = sum(FTAG),
        AwayGoalsAgainst = sum(FTHG)
    ) %>% 
    mutate(
        AwayGoalDifference = AwayGoalsFor -
            AwayGoalsAgainst,
        Points_away = AwayWin * 3 + AwayDraw
    ) %>% 
    arrange(
        desc(Points_away),
        desc(AwayGoalDifference)
    )

awayTeamDf %>% View()


## putting these tables together with a join

homeTeamDf
awayTeamDf

eplTable = inner_join(
    x = homeTeamDf,
    y = awayTeamDf,
    by = c("HomeTeam" = "AwayTeam")
) %>% 
    transmute(
        Team = HomeTeam,
        Wins = HomeWin + AwayWin,
        Draws = HomeDraw + AwayDraw,
        Losses = HomeLoss + AwayLoss,
        GoalsFor = HomeGoalsFor + AwayGoalsFor,
        GoalsAgainst = HomeGoalsAgainst + AwayGoalsAgainst,
        GoalDifference = GoalsFor - GoalsAgainst,
        Points = Wins * 3 + Draws
    ) %>% 
    arrange(
        desc(Points),
        desc(GoalDifference)
    )

eplTable %>% View()


#### Joins ####
## there are four types of joins: 

leftDf = tibble(
    Key = 1:4,
    LeftWord = paste0("l_val", Key)
)

rightDf = tibble(
    Key = 2:6,
    RightWord = paste0("r_val", Key)
)

leftDf
rightDf

## inner joins keeps records if we have records from both tables
inner_join(leftDf, rightDf, by = "Key")

## left/right joins
### left join keeps all records from the left table; populates missing records from right table with NA
left_join(leftDf, rightDf, by = "Key")

### symmetric operation for right table
right_join(leftDf, rightDf, by = "Key")

## this does the same thing
left_join(rightDf, leftDf, by = "Key")


## full (outer) join keeps all records
full_join(leftDf, rightDf, by = "Key")



### there are four ways tables can of match records: 1:1, 1:many, many:1, many:many
rightDf_2 = tibble(
    Key2 = sample(5, size = 8, replace = TRUE),
    RightWord = paste0("r_val", 1:8)
) 

## an example of a 1:many join
full_join(leftDf, rightDf_2, by = c("Key" = "Key2"))




