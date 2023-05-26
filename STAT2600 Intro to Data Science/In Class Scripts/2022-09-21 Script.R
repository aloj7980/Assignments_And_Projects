#### HW 2 ####
library(dplyr)
netflixDf <- read_csv("Homeworks/HW2/NetflixMoviesShows.csv")
netflixDf %>% View()

## a function that replaces `value` with `NA`
replace_value_with_na <- function(x, value){
    x[x == value] = NA
    return(x)
}

## in the netflix dataset the `director` column has "Not Given" for missing values.
replace_value_with_na(netflixDf$director[1:50], "Not Given")

## looking at the most popular director (w/o "Not Given" values)
sort(table(replace_value_with_na(netflixDf$director, "Not Given")))

## the piped version of this.
netflixDf$director %>% 
    replace_value_with_na("Not Given") %>% 
    table() %>% 
    sort(decreasing = TRUE) 

## use `select` to subset multiple columns
netflixDf %>% 
    select(
        duration, 
        director
    )

## you can use minus to drop the columns
netflixDf %>% 
    select(
        -duration, 
        -director
    )

#### Manipulating Rows ####
netflixDf %>% colnames()

## use the `filter` function to select certain rows. 
### need to pass in a logical vector; filter will return rows that are TRUE

## filters to just movie records
netflixDf %>% 
    filter(
        type == "Movie"
    )

### pass in two logicals and it will return rows that are both TRUE
### this returns the movies with the word "The" in the title 
netflixDf %>% 
    filter(
        type == "Movie",
        str_detect(title, "The")
    )


## arrange can be used to order by a column
netflixDf %>% 
    filter(
        type == "Movie"
    ) %>% 
    arrange(
        date_added
    )

## by default the column will order in ascending order, but can use `desc()` function to order in descending order
netflixDf %>% 
    filter(
        type == "Movie"
    ) %>% 
    arrange(
        desc(date_added)
    ) %>% View()

