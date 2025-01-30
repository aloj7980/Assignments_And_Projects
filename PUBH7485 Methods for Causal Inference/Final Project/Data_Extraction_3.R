# Required Libraries
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(airball)

# Read filtered_stats.csv
filtered_stats <- read.csv("filtered_stats.csv")

# Function to retrieve the player's Basketball-Reference URL
get_player_url <- function(player_name) {
  # Convert hyphens to spaces and remove other punctuation
  clean_name <- gsub("-", " ", player_name)       # Replace hyphens with spaces
  clean_name <- gsub("[[:punct:]]", "", clean_name)  # Remove remaining punctuation
  print(clean_name)
  # Construct the search URL using the cleaned name
  search_url <- paste0("https://www.basketball-reference.com/search/search.fcgi?search=", URLencode(clean_name))
  print(search_url)
  
  # Try to retrieve the search page
  search_page <- tryCatch(read_html(search_url), error = function(e) NULL)
  if (is.null(search_page)) {
    return(NA)
  }
  
  # Find the link within the `search-item-name` div
  player_link <- search_page %>%
    html_node(".search-item-name a") %>%
    html_attr("href")
  
  # If a link is found, create the full URL
  if (!is.na(player_link)) {
    full_url <- paste0("https://www.basketball-reference.com", player_link)
    return(full_url)
  } else {
    return(NA)
  }
}

# Get unique player and season combinations
unique_players_seasons <- filtered_stats %>%
  dplyr::select(Player, Season) %>%
  distinct()

# Create a data frame to store player URLs
player_urls <- data.frame(Player = unique(unique_players_seasons$Player), URL = NA, stringsAsFactors = FALSE)

# Get player URLs for each player
for (i in seq_along(player_urls$Player)) {
  player_urls$URL[i] <- get_player_url(player_urls$Player[i])
  print(player_urls$URL[i])
  Sys.sleep(2) # Pause to avoid overloading the server
}

# Update the URL column in player_urls based on manual corrections
player_urls <- player_urls %>%
  mutate(URL = case_when(
    Player == "Al Jefferson" ~ "https://www.basketball-reference.com/players/j/jeffeal01.html",
    Player == "Jakob Poeltl" ~ "https://www.basketball-reference.com/players/p/poeltja01.html",
    Player == "J.J. Hickson" ~ "https://www.basketball-reference.com/players/h/hicksjj01.html",
    Player == "J.J. Redick" ~ "https://www.basketball-reference.com/players/r/redicjj01.html",
    Player == "Isaiah Stewart II" ~ "https://www.basketball-reference.com/players/s/stewais01.html",
    Player == "Enes Kanter" ~ "https://www.basketball-reference.com/players/k/kanteen01.html",
    Player == "C.J. Miles" ~ "https://www.basketball-reference.com/players/m/milescj01.html",
    Player == "Bruce Brown Jr." ~ "https://www.basketball-reference.com/players/b/brownbr01.html",
    Player == "Kenyon Martin Jr." ~ "https://www.basketball-reference.com/players/m/martike04.html",
    Player == "O.J. Mayo" ~ "https://www.basketball-reference.com/players/m/mayooj01.html",
    Player == "Tim Hardaway Jr." ~ "https://www.basketball-reference.com/players/h/hardati02.html",
    Player == "John Wall" ~ "https://www.basketball-reference.com/players/w/walljo01.html",
    Player == "Elfrid Payton." ~ "https://www.basketball-reference.com/players/p/paytoel01.html",
    Player == "Gerald Henderson" ~ "https://www.basketball-reference.com/players/h/hendege02.html",
    Player == "Jerryd Bayless." ~ "https://www.basketball-reference.com/players/b/bayleje01.html",
    Player == "Glenn Robinson" ~ "https://www.basketball-reference.com/players/r/robingl02.html",
    Player == "Ivica Zubac" ~ "https://www.basketball-reference.com/players/z/zubaciv01.html",
    Player == "James Harden" ~ "https://www.basketball-reference.com/players/h/hardeja01.html",
    Player == "Jarell Martin" ~ "https://www.basketball-reference.com/players/m/martija01.html",
    Player == "Robert Williams" ~ "https://www.basketball-reference.com/players/w/williro04.html",
    TRUE ~ URL
  ))

# Initialize an empty list to store season data
seasons_data_list <- list()


for (i in seq_len(nrow(unique_players_seasons))) {
  tryCatch({
    # Read player stats page
    player <- unique_players_seasons$Player[i]
    season <- unique_players_seasons$Season[i]
    
    print(player)
    print(season)
    
    player_url <- player_urls$URL[player_urls$Player == player]
    print(player_url)
    player_page <- read_html(player_url)
    Sys.sleep(2) # Pause to avoid overloading the server
    
    # Extract season stats table
    stats_table <- player_page %>%
      html_node(xpath = '//*[@id="per_game_stats"]') %>%
      html_table()
    
    print(stats_table)
    
    # Filter to only seasons before the given season
    stats_table <- stats_table %>%
      filter(!is.na(Age), Season != "Career") %>%
      mutate(Season2 = Season) %>%
      separate(Season2, into = c("Start_Year", "End_Year"), sep = "-") %>%
      mutate(Start_Year = as.numeric(Start_Year)) %>%
      filter(Start_Year < as.numeric(substr(season, 1, 4))) %>%
      mutate(Player = player)
    
    stats_table <- stats_table %>%
      dplyr::select(
        Player, Season, G
      )
    print(stats_table)
    
    seasons_data_list[[length(seasons_data_list) + 1]] <- stats_table  
    }, error = function(e) {
    message(paste("Error processing:", player, season))
    return(NA)
  })
}

seasons_data <- bind_rows(seasons_data_list)

# Initialize an empty list to store injury data
injury_data_list <- list()

# Get unique player-season combinations
player_season_combos <- seasons_data %>%
  distinct(Player, Season)

# Loop through each player-season combo
for (i in seq_len(nrow(player_season_combos))) {
  player <- player_season_combos$Player[i]
  season <- player_season_combos$Season[i]
  
  # Extract season years
  season_first_year <- as.numeric(substr(season, 1, 4))
  season_second_year <- as.numeric(substr(season, 6, 7)) + 2000
  
  # Define start and end dates for the season
  start_date <- paste0(season_first_year, "-10-01")
  end_date <- paste0(season_second_year, "-04-30")
  
  if(season == '2019-20'){
    end_date <-  paste0(season_second_year, "-03-15")
  }
  if(season == '2020-21'){
    end_date <-  paste0(season_second_year, "-05-17")
  }
  if(season == '1998-99'){
    start_date <- "1999-10-01"
  }
  if(season == '2011-12'){
    start_date <- "2011-10-01"
  }
  
  print(paste("Fetching injuries for:", player, "Season:", season))
  
  # Fetch injury data for the player within the season range
  tryCatch({
    injury_data <- nba_injuries(
      start_date = start_date,
      end_date = end_date,
      player = player
    )
    
    # Add a column for Player and Season for tracking
    if (nrow(injury_data) > 0) {
      injury_data <- injury_data %>%
        mutate(Player = player, Season = season)
    }
    
    # Append to the list
    injury_data_list[[length(injury_data_list) + 1]] <- injury_data
    
  }, error = function(e) {
    message(paste("Error retrieving data for player:", player, "Season:", season))
    print(e)
  })
}

# Combine all injury data into a single dataframe
injury_data <- bind_rows(injury_data_list)



# Function to scrape game-by-game minutes for a player in a specific season
scrape_game_logs <- function(player_url, season) {
  # Remove ".html" from player_url if present
  player_url <- sub("\\.html$", "", player_url)

  # Convert the season format from "2015-16" to "2016" for the URL
  end_year <- as.integer(substr(season, 6, 7)) + 2000
  if(as.integer(substr(season, 6, 7)) > 50){
    end_year <- end_year-100
  }
  
  url <- paste0(player_url, "/gamelog/", end_year)
  
  # Try reading the page and handle errors if any
  page <- tryCatch(rvest::read_html(url), error = function(e) NULL)
  print(url)  # For debugging: prints the URL being accessed
  if (is.null(page)) return(NULL)
  
  # Attempt to extract the table with id "pgl_basic", handle missing table by returning NULL
  game_logs <- tryCatch(
    page %>%
      html_node("#pgl_basic") %>%
      html_table(fill = TRUE),
    error = function(e) NULL
  )
  
  # If the table is missing, return NULL to skip further processing
  if (is.null(game_logs)) return(NULL)
  
  # Ensure all columns have names
  colnames(game_logs) <- make.names(colnames(game_logs), unique = TRUE)
  
  # Filter out rows where 'Date' does not start with "2"
  game_logs <- game_logs %>%
    filter(str_starts(Date, "2") | str_starts(Date, "1")) %>%  # Keep rows where Date starts with "2"
    mutate(
      Season = season,  # Keep the original season format
      MP = sapply(str_split(MP, ":"), function(x) as.numeric(x[1]) + as.numeric(x[2]) * (1 / 60))  # Convert to decimal minutes
    ) %>%
    # Replace NA minutes with 0s for DNP games
    mutate(MP = ifelse(is.na(MP), 0, MP)) %>%
    dplyr::select(Date, MP, Season)  # Select only the necessary columns
  
  return(game_logs)
}

# Initialize an empty data frame to store all game logs
all_game_logs <- data.frame()

# Loop through each player-season combination and scrape game logs
for (i in seq_len(nrow(player_season_combos))) {
  player <- player_season_combos$Player[i]
  season <- player_season_combos$Season[i]  # Convert "2022-23" to "202223" format
  player_url <- player_urls$URL[player_urls$Player == player]
  print(player)
  print(season)
  if (!is.na(player_url)) {
    game_logs <- scrape_game_logs(player_url, season)
    print(game_logs)
    if (!is.null(game_logs)) {
      # Add player, season, and relevant team info from filtered_stats
      player_info <- filtered_stats %>%
        filter(Player == player, Season == player_season_combos$Season[i]) %>%
        distinct()  # Avoid duplicates
      
      game_logs <- game_logs %>%
        mutate(Player = player, Season = player_season_combos$Season[i]) %>%
        left_join(player_info, by = c("Player", "Season"))
      
      # Append to the combined data frame
      all_game_logs <- bind_rows(all_game_logs, game_logs)
    }
  }
  Sys.sleep(2) # Pause to avoid overloading the server
  
}




library(stringr)

# Ensure the Date columns are in Date format
all_game_logs <- all_game_logs %>%
  dplyr::select(Date, MP, Season, Player) %>%
  mutate(Date = as.Date(Date))

# Clean the 'Relinquished' and 'Acquired' columns by removing leading characters (like "• ")
injury_data <- injury_data %>%
  mutate(Relinquished = str_remove(Relinquished, "^•\\s*")) %>%
  mutate(Acquired = str_remove(Acquired, "^•\\s*"))

# Define the last game date for each season
season_end_dates <- all_game_logs %>%
  group_by(Season) %>%
  summarise(Last_Game_Date = max(Date))

# Initialize columns in all_game_logs
all_game_logs <- all_game_logs %>%
  mutate(Injury_Indicator = 0, Injury_Severity_Games = 0)

# Ensure the Injury_Notes column exists and is initialized with empty strings
all_game_logs$Injury_Notes <- ""

# Loop through each injury event
for (i in seq_len(nrow(injury_data))) {
  injury <- injury_data[i, ]
  player <- injury$Relinquished
  print(player)
  injury_date <- injury$Date
  season <- injury$Season
  injury_notes <- injury$Notes  # Get the injury notes
  print(season)
  print(injury_notes)
  
  # Skip rows without a player or injury date
  if (is.na(player) || is.na(injury_date)) next
  
  # Find the most recent game for the player within 7 days before the injury
  recent_game <- all_game_logs %>%
    filter(Player == player, MP > 0, Date <= injury_date, Date >= (injury_date - 7)) %>%
    arrange(desc(Date)) %>%
    slice(1)
  print(recent_game$Date)
  
  if (nrow(recent_game) == 0) next
  
  # Find the index of the recent game in all_game_logs
  game_index <- which(all_game_logs$Date == recent_game$Date & 
                        all_game_logs$Player == player)
  
  # Calculate the severity end date as the next game where the player played nonzero minutes
  next_game_played <- all_game_logs %>%
    filter(Player == player, Season == season, MP > 0, Date > injury_date) %>%
    arrange(Date) %>%
    slice(1) %>%
    pull(Date)
  
  # If no such game exists, use the last game date of the season
  severity_end_date <- ifelse(is.na(next_game_played) || length(next_game_played) == 0,
                              as.Date(season_end_dates %>% filter(Season == recent_game$Season) %>% pull(Last_Game_Date)),
                              next_game_played)
  severity_end_date <- as.Date(severity_end_date)
  severity_games <- num_rows <- all_game_logs %>%
    filter(Player == player, Season == season, Date > recent_game$Date, Date < severity_end_date) %>%
    nrow()
  print(severity_end_date)
  print(severity_games)
  
  # Update all_game_logs for the recent game
  all_game_logs$Injury_Indicator[game_index] <- 1
  all_game_logs$Injury_Severity_Games[game_index] <- severity_games
  all_game_logs$Injury_Notes[game_index] <- injury_notes
}

# Ensure games without injuries have empty strings in the Injury_Notes column
all_game_logs$Injury_Notes[is.na(all_game_logs$Injury_Indicator)] <- ""

# Verify the result
View(all_game_logs)



# Initialize the Games_Missed_Frequency_Career column
unique_players_seasons <- unique_players_seasons %>%
  mutate(Games_Missed_Frequency_Career = NA_real_)

# Loop through each player-season combination
for (i in seq_len(nrow(unique_players_seasons))) {
  # Get current player and season
  current_player <- unique_players_seasons$Player[i]
  current_season <- unique_players_seasons$Season[i]
  
  # Filter all_game_logs for seasons before the current season
  prior_seasons_logs <- all_game_logs %>%
    filter(Player == current_player, Season < current_season)
  
  # Skip if no prior seasons (rookies)
  if (nrow(prior_seasons_logs) == 0) {
    next
  }
  
  # Calculate total games missed due to injury
  total_games_missed <- sum(prior_seasons_logs$Injury_Severity, na.rm = TRUE)
  
  # Calculate total games played by the team in prior seasons
  total_team_games <- nrow(prior_seasons_logs)
  
  # Calculate the frequency of games missed due to injury
  games_missed_frequency <- total_games_missed / total_team_games
  
  # Assign the value to the Games_Missed_Frequency_Career column
  unique_players_seasons$Games_Missed_Frequency_Career[i] <- games_missed_frequency
}

# View the updated unique_players_seasons data frame
View(unique_players_seasons)



# Save the results
write.csv(unique_players_seasons, "career_injury_metrics.csv", row.names = FALSE)


