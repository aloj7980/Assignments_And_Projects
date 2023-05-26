library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

readFrame <- read_csv("NetflixMoviesShows.csv")
View(readFrame)
people <- readFrame$director
people <- people[!people %in% c("Not Given")]
people <- sort(people)
people <- unique(people)


PersonTable <- data.frame(PersonKey = 1:length(people),
                          Name = people)
AllTable <- right_join(x=PersonTable, 
                         y=readFrame, 
                         by=c("Name" = "director")) %>% 
  transmute(Title = title,
            Type = type,
            DirectorKey = PersonKey,
            Country = country,
            DateAdded = as.Date(date_added,format="%m/%d/%Y"),
            ReleaseYear = release_year,
            Rating = rating,
            Duration = as.numeric(sapply(duration,str_extract,"[0-9]+")))
MovieTable <- AllTable[which(AllTable$Type=="Movie"), ] %>% subset(select = -c(Type)) %>% arrange(DateAdded)
View(MovieTable)
ShowTable <- AllTable[which(AllTable$Type=="TV Show"), ] %>% subset(select = -c(Type)) %>% rename(Seasons = Duration) %>% arrange(DateAdded)
View(ShowTable)
View(PersonTable)

MovieTable %>% write_csv("MovieTable.csv")
ShowTable %>% write_csv("ShowTable.csv")
PersonTable %>% write_csv("PersonTable.csv")

