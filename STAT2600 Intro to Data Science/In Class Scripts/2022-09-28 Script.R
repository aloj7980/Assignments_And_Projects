library(tidyverse)

#### Pivoting ####
## let's walk through the vignette
vignette("pivot")
library(tidyr)
library(dplyr)
library(readr)

relig_income

relig_income %>% 
    pivot_longer(
        cols = !religion, 
        names_to = "income",
        values_to = "count"
    ) %>% 
    mutate(
        income_numeric = ifelse(
            income == "<$10k", 5, 
            income %>% 
                str_extract("[[:digit:]]+") %>% 
                as.numeric()
        ),
        total_income = count*income_numeric
    ) %>% 
    group_by(
        religion
    ) %>% 
    summarise(
        N = sum(count),
        TotalIncome = sum(total_income, na.rm = TRUE)
    ) %>% 
    mutate(
        AvIncome = TotalIncome/N
    ) %>% arrange(
        desc(AvIncome)
    )



billboard

billboard %>% 
    pivot_longer(
        cols = starts_with("wk"), 
        names_to = "week", 
        values_to = "rank",
        values_drop_na = TRUE
    )


### pivot_wider
fish_encounters
fish_encounters %>% 
    pivot_wider(
        names_from = station, 
        values_from = seen)

fish_encounters %>% 
    group_by(
        station
    ) %>% 
    summarise(
        N = n()
    )


#### `select` helpers ####
eplDf = read_csv("Data/epl2122.csv")
eplDf %>% View()
eplDf %>% colnames()

eplDf %>% 
    select(
        starts_with("H")
    )

eplDf %>% 
    select(
        contains("S")
    )

eplDf %>% 
    select(
        contains("S")
    )


#### sumarize_all #### 

vignette("colwise")

### can i get the minimum value for all the numeric columns
eplDf 

## the long/painful way to do this
eplDf %>% 
    summarise(
        min(FTHG),
        min(FTAG),
        min(HTHG)
    )

## too painful

eplDf %>% 
    summarise(
        across(where(is.numeric), min)
    )

## what about the mean for each of these columns
eplDf %>% 
    summarise(
        across(
            .cols = where(is.numeric), 
            .fns = list(
                 mean, Min = min, Max = max)
        )
    ) %>% 
    select(
        ends_with("Mean"),
        ends_with("Min"),
        ends_with("Max")
    )


mean(eplDf$FTHG)

## which columns are numeric
colnames(eplDf)[sapply(eplDf, is.numeric)]

