# Read filtered_stats
filtered_stats <- read.csv("filtered_stats.csv", stringsAsFactors = FALSE)

# Read filtered_stats_accents
filtered_stats_accents <- read.csv("filtered_stats_accents.csv", stringsAsFactors = FALSE)

# Read all_game_logs
all_game_logs <- read.csv("all_game_logs.csv", stringsAsFactors = FALSE)

# Read travel_data
travel_data <- read.csv("travel_data.csv", stringsAsFactors = FALSE)

# Read injury_data
injury_data <- read.csv("injury_data.csv", stringsAsFactors = FALSE)

# Print confirmation
cat("All CSV files have been read successfully.\n")


# Step 1: Ensure consistency of player names between data sets

library(dplyr)

# Define a mapping of mismatched player names
name_mapping <- list(
  "Bogdan Bogdanovic" = "Bogdan Bogdanović",
  "Bojan Bogdanovic" = "Bojan Bogdanović",
  "Bruce Brown Jr." = "Bruce Brown",
  "C.J. McCollum" = "CJ McCollum",
  "C.J. Miles" = "CJ Miles",
  "Dante Exum" = "Danté Exum",
  "Danuel House" = "Danuel House Jr.",
  "Dario Saric" = "Dario Šarić",
  "Dennis Schroder" = "Dennis Schröder",
  "Dennis Smith" = "Dennis Smith Jr.",
  "Derrick Jones" = "Derrick Jones Jr.",
  "Enes Kanter" = "Enes Freedom",
  "Glenn Robinson" = "Glenn Robinson III",
  "Herb Jones" = "Herbert Jones",
  "Isaiah Stewart II" = "Isaiah Stewart",
  "J.J. Hickson" = "JJ Hickson",
  "J.J. Redick" = "JJ Redick",
  "J.R. Smith" = "JR Smith",
  "Jakob Poeltl" = "Jakob Pöltl",
  "James Ennis" = "James Ennis III",
  "Jaren Jackson, Jr." = "Jaren Jackson Jr.",
  "Jonas Valanciunas" = "Jonas Valančiūnas",
  "Jusuf Nurkic" = "Jusuf Nurkić",
  "Kenyon Martin Jr." = "KJ Martin",
  "Kevin Knox" = "Kevin Knox II",
  "Luka Doncic" = "Luka Dončić",
  "Kristaps Porzingis" = "Kristaps Porziņģis",
  "Lonnie Walker" = "Lonnie Walker IV",
  "Marcus Morris" = "Marcus Morris Sr.",
  "Mohamed Bamba" = "Mo Bamba",
  "Monte Morris" = "Monté Morris",
  "Nene Hilario" = "Nene",
  "Nicolas Claxton" = "Nic Claxton",
  "Nikola Jokic" = "Nikola Jokić",
  "Nikola Vucevic" = "Nikola Vučević",
  "Otto Porter" = "Otto Porter Jr.",
  "R.J. Barrett" = "RJ Barrett",
  "Raulzinho Neto" = "Raul Neto",
  "Reggie Bullock" = "Reggie Bullock Jr.",
  "Robert Williams" = "Robert Williams III",
  "Xavier Tillman, Sr." = "Xavier Tillman"
)

# Reverse the name mapping to match names in all_game_logs
reverse_name_mapping <- setNames(names(name_mapping), name_mapping)

# Apply the reverse name mapping to travel_data
travel_data <- travel_data %>%
  mutate(Player = ifelse(Player %in% names(reverse_name_mapping), reverse_name_mapping[Player], Player))

# Apply the reverse name mapping to injury_data
injury_data <- injury_data %>%
  mutate(Player = ifelse(Player %in% names(reverse_name_mapping), reverse_name_mapping[Player], Player))

# Define the name corrections
name_corrections <- c(
  "Kelly Oubre, Jr." = "Kelly Oubre Jr.",
  "Moe Harkless" = "Maurice Harkless",
  "Wendell Carter, Jr." = "Wendell Carter Jr."
)

# Function to apply name corrections
correct_player_names <- function(data, corrections) {
  data %>%
    mutate(Player = if_else(Player %in% names(corrections), corrections[Player], Player))
}

# Apply corrections to both data frames
filtered_stats <- correct_player_names(filtered_stats, name_corrections)
all_game_logs <- correct_player_names(all_game_logs, name_corrections)




# Step 2: Add travel data to all_game_logs

# Extract unique player-season combinations from filtered_stats
filtered_combos <- filtered_stats %>%
  dplyr::select(Player, Season) %>%
  distinct()

# Extract unique player-season combinations from travel_data
travel_combos <- travel_data %>%
  dplyr::select(Player, Season) %>%
  distinct()

# Find missing player-season combinations
missing_combos <- filtered_combos %>%
  anti_join(travel_combos, by = c("Player", "Season"))

# Display the missing combinations
if (nrow(missing_combos) > 0) {
  cat("Missing player-season combinations:\n")
  print(missing_combos)
} else {
  cat("All player-season combinations in filtered_stats are present in travel_data.\n")
}

# Add the 'Travel' and 'Distance' columns to all_game_logs
all_game_logs <- all_game_logs %>%
  left_join(
    travel_data %>%
      mutate(
        Travel = if_else(Route == "No Travel", 0, 1),  # Create the Travel column
        Distance = as.numeric(Distance)               # Ensure Distance is numeric
      ) %>%
      select(Player, Season, Date, Travel, Distance),  # Select necessary columns
    by = c("Player", "Season", "Date")                # Match on Player, Season, and Date
  )

all_game_logs <- all_game_logs %>%
  mutate(
    Travel = if_else(is.na(Travel), 0, Travel),        # Replace NA in Travel with 0
    Distance = if_else(is.na(Distance), 0, Distance)  # Replace NA in Distance with 0
  )

# View the updated all_game_logs
View(all_game_logs)



# Step 3: Add injury Data to all_game_logs

# We need to get a few injury transactions from 2020-21 that were omitted because the season ended later

library(airball)

# Filter the players for the 2020-21 season
players_2021 <- filtered_stats_accents %>%
  filter(Season == "2020-21") %>%
  distinct(Player)

# Define the specific date range for 2021
start_date <- "2021-04-30"
end_date <- "2021-05-17"

# Initialize an empty list for the injuries
injury_data_list <- list()

# Loop through each player in the 2020-21 season
for (i in seq_len(nrow(players_2021))) {
  player <- players_2021$Player[i]
  print(paste("Fetching injuries for:", player, "within", start_date, "to", end_date))
  
  # Retrieve injury data for the specific date range
  tryCatch({
    injury_data_player <- nba_injuries(
      start_date = start_date,
      end_date = end_date,
      player = player
    )
    
    # Add player and season columns if there are injuries
    if (nrow(injury_data_player) > 0) {
      injury_data_player <- injury_data_player %>%
        mutate(Player = player, Season = "2020-21")
      
      # Append to the list
      injury_data_list[[length(injury_data_list) + 1]] <- injury_data_player
    }
  }, error = function(e) {
    message(paste("Error retrieving data for player:", player))
    print(e)
  })
}

# Combine all retrieved injury data into a single dataframe
additional_injuries_2021 <- bind_rows(injury_data_list)

# Display the final injury data for verification
View(additional_injuries_2021)

# Ensure that the Date column is in Date format
injury_data <- injury_data %>%
  mutate(Date = as.Date(Date))

# Combine the additional_injuries_2021 with injury_data
injury_data <- bind_rows(injury_data, additional_injuries_2021)

# Apply the reverse name mapping to injury_data again because new rows were added in 2021
injury_data <- injury_data %>%
  mutate(Player = ifelse(Player %in% names(reverse_name_mapping), reverse_name_mapping[Player], Player))

View(injury_data)



library(stringr)

# Remove bubble seeding games from all_game_logs
all_game_logs <- all_game_logs %>%
  filter(!(Season == "2019-20" & Date > as.Date("2020-03-31")))

# Ensure the Date columns are in Date format
all_game_logs <- all_game_logs %>%
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
  mutate(Injury_Indicator = 0, Injury_Severity = 0)

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
  severity_days <- as.integer(severity_end_date - recent_game$Date)
  print(severity_end_date)
  print(severity_days)
  
  # Update all_game_logs for the recent game
  all_game_logs$Injury_Indicator[game_index] <- 1
  all_game_logs$Injury_Severity[game_index] <- severity_days
  all_game_logs$Injury_Notes[game_index] <- injury_notes
}

# Ensure games without injuries have empty strings in the Injury_Notes column
all_game_logs$Injury_Notes[is.na(all_game_logs$Injury_Indicator)] <- ""

# Verify the result
View(all_game_logs)







# Step 4: Add additional calculated variables (arbitrary density indicators)

# Ensure all_game_logs has columns for position indicators, initialize them to 0
position_columns <- c("PG", "SG", "SF", "PF", "C")
for (col in position_columns) {
  all_game_logs[[col]] <- 0
}

# Loop through filtered_stats to assign positions
for (i in seq_len(nrow(filtered_stats))) {
  player <- filtered_stats$Player[i]
  season <- filtered_stats$Season[i]
  position <- filtered_stats$Position[i]  # Assuming this column contains the positions
  print(position)
  # If the position information is missing, skip
  if (is.na(position)) next
  
  # Filter all_game_logs for the current player and season
  player_season_games <- all_game_logs %>%
    filter(Player == player, Season == season)
  
  # Get row indices for the filtered games
  game_indices <- which(all_game_logs$Player == player & all_game_logs$Season == season)
  
  # Update position indicators for the filtered games
  if (position %in% position_columns) {
    all_game_logs[game_indices, position] <- 1
  }
}

# Add indicator variables for seasons influenced by the pandemic

# Ensure all_game_logs has columns for the season indicators, initialize them to 0
pandemic_seasons <- c("2019-20", "2020-21", "2021-22")
for (season in pandemic_seasons) {
  col_name <- paste0("Is_", gsub("-", "_", season))  # Create column names like Is_2019_20
  all_game_logs[[col_name]] <- 0
}

# Update the indicators for each game in the relevant seasons
for (season in pandemic_seasons) {
  col_name <- paste0("Is_", gsub("-", "_", season))
  all_game_logs[[col_name]] <- ifelse(all_game_logs$Season == season, 1, 0)
}



# Initialize the new columns to 0
all_game_logs <- all_game_logs %>%
  mutate(
    Is_3rd_Game_in_4_Days = 0,
    Is_4th_Game_in_5_Days = 0,
    Is_5th_Game_in_7_Days = 0
  )

# Group by Player to calculate indicators per player
all_game_logs <- all_game_logs %>%
  arrange(Player, Date) %>% # Ensure data is ordered correctly
  group_by(Player) %>%
  mutate(
    # Rolling count of games with MP > 0 in the last 5 days
    Games_in_5_Days = sapply(
      row_number(),
      function(i) sum(MP[Date >= (Date[i] - 4) & Date <= Date[i]] > 0)
    ),
    Is_4th_Game_in_5_Days = ifelse(Games_in_5_Days == 4, 1, 0),
    
    # Rolling count of games with MP > 0 in the last 4 days
    Games_in_4_Days = sapply(
      row_number(),
      function(i) sum(MP[Date >= (Date[i] - 3) & Date <= Date[i]] > 0)
    ),
    Is_3rd_Game_in_4_Days = ifelse(Games_in_4_Days == 3, 1, 0),
    
    # Rolling count of games with MP > 0 in the last 7 days
    Games_in_7_Days = sapply(
      row_number(),
      function(i) sum(MP[Date >= (Date[i] - 6) & Date <= Date[i]] > 0)
    ),
    Is_5th_Game_in_7_Days = ifelse(Games_in_7_Days == 5, 1, 0)
  ) %>%
  ungroup() %>%
  select(-Games_in_5_Days, -Games_in_4_Days, -Games_in_7_Days) # Remove intermediate columns

View(all_game_logs)




# Step 5: Get back to back pairs

# Create the b2b_pairs data frame with all columns from all_game_logs
b2b_pairs <- all_game_logs %>%
  arrange(Player, Date) %>% # Ensure data is sorted by player and date
  group_by(Player) %>% # Group by Player to handle each player's games separately
  mutate(
    Next_Game_Date = lead(Date), # Get the next game's date
    Is_B2B_Next = as.integer((Next_Game_Date - Date) == 1) # Identify if next game is part of a back-to-back
  ) %>%
  mutate(
    pair_id_within_player = cumsum(coalesce(Is_B2B_Next, 0)) # Create unique pair_id within each player
  ) %>%
  mutate(
    pair_id_within_player = if_else(Is_B2B_Next == 1 | lag(Is_B2B_Next == 1, default = 0), pair_id_within_player, NA_integer_) # Assign same pair_id to both games in a B2B
  ) %>%
  filter(!is.na(pair_id_within_player)) %>% # Keep only games that are part of back-to-backs
  ungroup() %>% # Ungroup to assign a global pair_id
  mutate(
    pair_id = dense_rank(interaction(Player, pair_id_within_player)) # Generate globally unique pair_id
  ) %>%
  select(-Next_Game_Date, -Is_B2B_Next, -pair_id_within_player) # Drop helper columns

# Verify the result
View(b2b_pairs)

# Filter for back-to-back pairs where:
# - One game was played and the other was missed
b2b_played_not_played <- b2b_pairs %>%
  group_by(pair_id) %>%
  filter(
    sum(MP > 0) == 1 && sum(MP == 0) == 1
  ) %>%
  ungroup()

# Verify the result
View(b2b_played_not_played)

# Create columns for Prev_Game_MP, Next_Game_MP, and Prev_Game_Injury_Indicator
b2b_played_not_played$Prev_Game_MP <- NA
b2b_played_not_played$Next_Game_MP <- NA
b2b_played_not_played$Prev_Game_Injury_Indicator <- NA

# Loop through each row in b2b_played_not_played
for (i in seq_len(nrow(b2b_played_not_played))) {
  # Get the current player and date
  current_player <- b2b_played_not_played$Player[i]
  current_date <- b2b_played_not_played$Date[i]
  
  # Debugging: Print the current player and date
  print(paste("Processing Player:", current_player, "Date:", current_date))
  
  # Find the previous game
  prev_game <- all_game_logs %>%
    filter(Player == current_player, Date < current_date) %>%
    arrange(desc(Date)) %>%
    slice(1)
  
  # Update Prev_Game_MP and Prev_Game_Injury_Indicator
  if (nrow(prev_game) == 0) {
    print("No previous game found. Setting Prev_Game_MP and Prev_Game_Injury_Indicator to 0.")
    b2b_played_not_played$Prev_Game_MP[i] <- 0
    b2b_played_not_played$Prev_Game_Injury_Indicator[i] <- 0
  } else {
    print(paste("Previous game MP:", prev_game$MP, "Injury Indicator:", prev_game$Injury_Indicator))
    b2b_played_not_played$Prev_Game_MP[i] <- prev_game$MP
    b2b_played_not_played$Prev_Game_Injury_Indicator[i] <- prev_game$Injury_Indicator
  }
  
  # Find the next game
  next_game <- all_game_logs %>%
    filter(Player == current_player, Date > current_date) %>%
    arrange(Date) %>%
    slice(1)
  
  # Update Next_Game_MP
  if (nrow(next_game) == 0) {
    print("No next game found. Setting Next_Game_MP to 0.")
    b2b_played_not_played$Next_Game_MP[i] <- 0
  } else {
    print(paste("Next game MP:", next_game$MP))
    b2b_played_not_played$Next_Game_MP[i] <- next_game$MP
  }
}

# Verify the updated b2b_played_not_played dataframe
View(b2b_played_not_played)

# Filter pairs where:
# - First game was missed, the player had MP > 0 in the previous game, and an Injury_Indicator of 1 in the previous game
# - Second game was missed, the player had MP > 0 in the next game
load_management_b2b <- b2b_played_not_played %>%
  group_by(Player, pair_id) %>%
  filter(
    any((row_number() == 1 & MP == 0 & Prev_Game_MP > 0 & Prev_Game_Injury_Indicator == 1) | 
          (row_number() == 2 & MP == 0 & Next_Game_MP > 0))
  ) %>%
  ungroup()

# Verify the final result
View(load_management_b2b)














# Step 6: Remove injuries associated with load management

previous_game_indices <- c()
# Loop through each row in load_management_b2b
for (i in seq_len(nrow(load_management_b2b))) {
  # Get the current player's name and the pair ID
  current_player <- load_management_b2b$Player[i]
  current_pair_id <- load_management_b2b$pair_id[i]
  
  # Find the game in load_management_b2b where the player did not play
  missed_game <- load_management_b2b %>%
    filter(Player == current_player, pair_id == current_pair_id, MP == 0) %>%
    slice(1)  # There should only be one missed game per pair_id

  if (nrow(missed_game) == 0) next  # Skip if no missed game is found
  
  # Get the missed game's date
  missed_game_date <- missed_game$Date
  print(missed_game_date)
  
  # Find the game before the missed game in all_game_logs
  previous_game <- all_game_logs %>%
    filter(Player == current_player, Date < missed_game_date) %>%
    arrange(desc(Date)) %>%
    slice(1)
  
  if (nrow(previous_game) == 0) next  # Skip if no previous game is found
  
  # Find the index of the previous game in all_game_logs
  previous_game_index <- which(all_game_logs$Player == current_player &
                                 all_game_logs$Date == previous_game$Date)
  previous_game_indices <- append(previous_game_indices, previous_game_index)
  
  # Set Injury_Indicator and Injury_Severity to 0 for the previous game
  all_game_logs$Injury_Indicator[previous_game_index] <- 0
  all_game_logs$Injury_Severity[previous_game_index] <- 0
  
  # Optional: Debugging information
  print(paste("Updated injury for Player:", current_player,
              "Date:", previous_game$Date,
              "Pair ID:", current_pair_id))
}

# Verify updates
all_game_logs %>%
  slice(previous_game_indices) %>%
  View()



# Step 7: Get injury metrics (Injury frequency and games missed frequency) which will be the outcomes of interest

# Create an empty list to store the results
injury_metrics_list <- list()

# Get unique Player-Season combinations
player_season_combos <- all_game_logs %>%
  distinct(Player, Season)

# Loop through each Player-Season combination
for (i in seq_len(nrow(player_season_combos))) {
  # Extract the Player and Season
  current_player <- player_season_combos$Player[i]
  current_season <- player_season_combos$Season[i]
  print(current_player)
  print(current_season)
  
  # Filter data for the current Player and Season
  player_season_logs <- all_game_logs %>%
    filter(Player == current_player, Season == current_season)
  
  # Calculate Injury Frequency
  injury_frequency <- sum(player_season_logs$Injury_Indicator == 1 & player_season_logs$MP > 0) / 
    sum(player_season_logs$MP > 0)
  
  # Calculate Games Missed
  games_missed <- 0
  for (j in which(player_season_logs$Injury_Indicator == 1)) {
    # Get the injury severity in days for the current injury
    severity_days <- player_season_logs$Injury_Severity[j]
    print("Days missed:")
    print(severity_days)
    # Get the injury date and team
    injury_date <- player_season_logs$Date[j]
    team <- player_season_logs$Team[j]
    
    # Find games missed due to the injury
    games_missed <- games_missed + all_game_logs %>%
      filter(
        Player == current_player,
        Date > injury_date & Date <= injury_date + severity_days - 1
      ) %>%
      nrow()
    print("Total Games missed:")
    print(games_missed)
  }
  
  # Calculate Total Games and Games Missed Frequency
  total_games <- nrow(player_season_logs)
  games_missed_frequency <- games_missed / total_games
  
  print(injury_frequency)
  print(games_missed_frequency)
  
  # Append the results to the list
  injury_metrics_list[[i]] <- data.frame(
    Player = current_player,
    Season = current_season,
    Injury_Frequency = injury_frequency,
    Games_Missed = games_missed,
    Total_Games = total_games,
    Games_Missed_Frequency = games_missed_frequency
  )
}

# Combine the list into a single data frame
injury_metrics <- do.call(rbind, injury_metrics_list)

# View the resulting data frame
View(injury_metrics)







# Step 8: get back to backs dataset with treatment indicator

#Create the treatment indicator for load management back-to-backs
load_management_b2b <- load_management_b2b %>%
  mutate(treatment = 1)

#Create the treatment indicator for played-both back-to-backs
b2b_played_both <- b2b_pairs %>%
  group_by(pair_id) %>%
  filter(sum(MP > 0) == 2) %>%
  mutate(treatment = 0) %>%
  ungroup()

#Combine load_management_b2b and b2b_played_both
combined_b2b <- bind_rows(load_management_b2b, b2b_played_both)

#Set distance = 0 when game not played
combined_b2b <- combined_b2b %>%
  mutate(Distance = ifelse(MP == 0, 0, Distance))

#Extract first and second games for each pair
first_games <- combined_b2b  %>% 
  group_by(pair_id) %>%
  filter(Date == min(Date)) %>% 
  slice(1) %>%
  ungroup() 

second_games <- combined_b2b %>% 
  group_by(pair_id) %>%
  filter(Date == max(Date)) %>% 
  slice(1) %>%
  ungroup() 

#Merge first and second games for each pair
b2b_of_interest <- first_games %>%
  left_join(
    second_games,
    by = "pair_id"
  )

#Add Travel and Distance modifications
b2b_of_interest <- b2b_of_interest %>%
  mutate(
    Travel_G1 = Travel.x,                     # Travel from the first game
    Travel_G2 = Travel.y,                     # Travel from the second game
    Distance_Sum = Distance.x + Distance.y    # Sum of distances from both games
  ) %>%
  # Select the desired columns
  select(
    Travel_G1, Travel_G2, Distance_Sum,       # Travel and summed distance
    ends_with(".y")                           # All columns from the second game
  ) %>%
  # Rename .y columns by removing the suffix
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

#This gets the distance per game played in the b2b
b2b_of_interest <- b2b_of_interest %>%
  mutate(distance_average = ifelse(treatment == 0, Distance_Sum / 2, Distance_Sum))

b2b_of_interest <- b2b_of_interest %>%
  # Drop unwanted columns
  select(-c(Prev_Game_MP, Next_Game_MP, Prev_Game_Injury_Indicator, Travel_G1, Travel_G2, Distance_Sum))


# View the resulting data frame
View(b2b_of_interest)


# Recalculate the game density indicators for load_management_b2b so that it's based on the given indicator would've occurred if not for the load management

# Loop through each back-to-back pair in b2b_of_interest where treatment = 1
for (i in seq_len(nrow(b2b_of_interest))) {
  # Skip if the treatment is not 1
  if (b2b_of_interest$treatment[i] != 1) next
  
  # Get the current player and the date of the first game in the back-to-back pair
  current_player <- b2b_of_interest$Player[i]
  first_game_date <- b2b_of_interest$Date[i] - 1 # Assuming the first game is one day before the second game
  
  print(current_player)
  print(first_game_date)
  
  # Get the indices of games before the first game of the back-to-back in all_game_logs
  games_before_b2b <- all_game_logs %>%
    filter(Player == current_player, Date < first_game_date) %>%
    arrange(desc(Date))
  
  # Update Is_3rd_Game_in_4_Days (1 game in the 2 days before the first game)
  if (nrow(games_before_b2b) >= 1) {
    games_in_2_days <- games_before_b2b %>%
      filter(Date >= (first_game_date - 2) & Date < first_game_date)
    if (nrow(games_in_2_days) == 1) {
      all_game_logs <- all_game_logs %>%
        mutate(Is_3rd_Game_in_4_Days = ifelse(
          Player == current_player & Date == first_game_date + 1, 1, Is_3rd_Game_in_4_Days
        ))
        print('3')
    }
  }
  
  # Update Is_4th_Game_in_5_Days (2 games in the 3 days before the first game)
  if (nrow(games_before_b2b) >= 2) {
    games_in_3_days <- games_before_b2b %>%
      filter(Date >= (first_game_date - 3) & Date < first_game_date)
    if (nrow(games_in_3_days) == 2) {
      all_game_logs <- all_game_logs %>%
        mutate(Is_4th_Game_in_5_Days = ifelse(
          Player == current_player & Date == first_game_date + 1, 1, Is_4th_Game_in_5_Days
        ))
        print('4')
    }
  }
  
  # Update Is_5th_Game_in_7_Days (3 games in the 5 days before the first game)
  if (nrow(games_before_b2b) >= 3) {
    games_in_5_days <- games_before_b2b %>%
      filter(Date >= (first_game_date - 5) & Date < first_game_date)
    if (nrow(games_in_5_days) == 3) {
      all_game_logs <- all_game_logs %>%
        mutate(Is_5th_Game_in_7_Days = ifelse(
          Player == current_player & Date == first_game_date + 1, 1, Is_5th_Game_in_7_Days
        ))
        print('5')
    }
  }
}

# Add updated density indicators to b2b_of_interest
b2b_of_interest <- b2b_of_interest %>%
  left_join(
    all_game_logs %>%
      select(Player, Date, Is_3rd_Game_in_4_Days, Is_4th_Game_in_5_Days, Is_5th_Game_in_7_Days),
    by = c("Player", "Date")
  )

# Rename .y columns and drop .x columns
b2b_of_interest <- b2b_of_interest %>%
  dplyr::select(
    -ends_with(".x"),  # Remove all .x columns
  ) %>%
  rename_with(
    ~ gsub("\\.y$", "", .),  # Remove .y suffix from column names
    ends_with(".y")          # Apply rename only to columns ending with .y
  )




# Get the rolling_average_mp from before the b2b pair
b2b_of_interest <- b2b_of_interest %>%
  rowwise() %>%
  mutate(
    Rolling_Avg_MP = {
      # Identify the first game of the pair
      current_player = Player
      current_date = Date
      first_game <- all_game_logs %>%
        filter(
          Player == current_player,
          Date < current_date
        ) %>%
        arrange(desc(Date)) %>%
        slice(1)
      
      print(Player)
      print(first_game$Date)
      
      # Check if first_game is empty
      if (nrow(first_game) == 0) {
        # If no first game exists, retain the original Rolling_Avg_MP
        Rolling_Avg_MP
      } else {
        # Find the game before the first game of the pair
        prev_game <- all_game_logs %>%
          filter(
            Player == current_player,
            Date < first_game$Date
          ) %>%
          arrange(desc(Date)) %>%
          slice(1)
        
        print(prev_game$Date)
        print(prev_game$Rolling_Avg_MP)
        
        # Use the Rolling_Avg_MP of that game, or retain the original if no previous game
        if (nrow(prev_game) > 0) {
          prev_game$Rolling_Avg_MP
        } else {
          Rolling_Avg_MP
        }
      }
    }
  ) %>%
  ungroup()

# Verify the updated b2b_of_interest
View(b2b_of_interest)













#Step 9: Get categories of treatment

# Add a column for game order within each player-season
all_game_logs <- all_game_logs %>%
  arrange(Player, Season, Date) %>% # Ensure proper ordering
  group_by(Player, Season) %>%
  mutate(Game_Order = row_number()) %>%
  ungroup()

# Assign early, mid, late season based on thirds of total games
all_game_logs <- all_game_logs %>%
  group_by(Player, Season) %>%
  mutate(
    Total_Games = n(),
    Season_Phase = case_when(
      Game_Order <= Total_Games / 3 ~ "Early",
      Game_Order > Total_Games / 3 & Game_Order <= 2 * Total_Games / 3 ~ "Mid",
      TRUE ~ "Late"
    )
  ) %>%
  ungroup()

# Assign Season_Phase to b2b_of_interest
b2b_of_interest <- b2b_of_interest %>%
  left_join(
    all_game_logs %>% select(Player, Season, Date, Season_Phase),
    by = c("Player", "Season", "Date")
  )

# Calculate treatment frequencies without summarise
b2b_of_interest <- b2b_of_interest %>%
  mutate(
    Early_Treatment = ifelse(Season_Phase == "Early", treatment, 0),
    Mid_Treatment = ifelse(Season_Phase == "Mid", treatment, 0),
    Late_Treatment = ifelse(Season_Phase == "Late", treatment, 0)
  )

# Compute cumulative treatment frequencies for each phase
b2b_of_interest <- b2b_of_interest %>%
  group_by(Player, Season) %>%
  mutate(
    Total_Early_Treatment = cumsum(Early_Treatment),
    Total_Mid_Treatment = cumsum(Mid_Treatment),
    Total_Late_Treatment = cumsum(Late_Treatment)
  ) %>%
  ungroup()

# Add the number of rows in each phase for each player and season
b2b_of_interest <- b2b_of_interest %>%
  group_by(Player, Season, Season_Phase) %>%
  mutate(Rows_In_Phase = n()) %>%
  ungroup() %>%
  group_by(Player, Season) %>%
  mutate(Total_Games = n()) %>%
  ungroup()

# Calculate treatment frequencies for early, mid, and late phases
b2b_of_interest <- b2b_of_interest %>%
  mutate(
    Frequency_Early = ifelse(Season_Phase == "Early", Total_Early_Treatment / Rows_In_Phase, NA),
    Frequency_Mid = ifelse(Season_Phase == "Mid", Total_Mid_Treatment / Rows_In_Phase, NA),
    Frequency_Late = ifelse(Season_Phase == "Late", Total_Late_Treatment / Rows_In_Phase, NA)
  )

# Calculate total treatment frequency for the entire season
b2b_of_interest <- b2b_of_interest %>%
  mutate(
    Total_Treatment_Frequency = (Total_Early_Treatment + Total_Mid_Treatment + Total_Late_Treatment) / Total_Games
  )

# Propagate phase-specific frequencies across all rows for each player-season
b2b_of_interest <- b2b_of_interest %>%
  group_by(Player, Season) %>%
  mutate(
    Frequency_Early = max(Frequency_Early, na.rm = TRUE),
    Frequency_Mid = max(Frequency_Mid, na.rm = TRUE),
    Frequency_Late = max(Frequency_Late, na.rm = TRUE),
    Total_Treatment_Frequency = max(Total_Treatment_Frequency, na.rm = TRUE)
  ) %>%
  ungroup()



treatment_categories <- b2b_of_interest %>%
  group_by(Player, Season) %>%
  slice_tail(n = 1) %>%
  select(Player, Season, Frequency_Early, Frequency_Mid, Frequency_Late, Total_Treatment_Frequency)



# View the resulting data frame
View(treatment_categories)

cat("Distribution of Total_Early_Treatment:\n")
hist(treatment_categories$Frequency_Early)

cat("\nDistribution of Total_Mid_Treatment:\n")
hist(treatment_categories$Frequency_Mid)

cat("\nDistribution of Total_Late_Treatment:\n")
hist(treatment_categories$Frequency_Late)

cat("\nDistribution of Total_Late_Treatment:\n")
hist(treatment_categories$Total_Treatment_Frequency)



# Precompute nonzero medians for each column
early_non_zero_median <- treatment_categories$Frequency_Early[treatment_categories$Frequency_Early > 0] %>%
  quantile(probs = 0.5, na.rm = TRUE)

mid_non_zero_median <- treatment_categories$Frequency_Mid[treatment_categories$Frequency_Mid > 0] %>%
  quantile(probs = 0.5, na.rm = TRUE)

late_non_zero_median <- treatment_categories$Frequency_Late[treatment_categories$Frequency_Late > 0] %>%
  quantile(probs = 0.5, na.rm = TRUE)

total_non_zero_median <- treatment_categories$Total_Treatment_Frequency[treatment_categories$Total_Treatment_Frequency > 0] %>%
  quantile(probs = 0.5, na.rm = TRUE)


# Add quantile categories based on precomputed medians
treatment_categories <- treatment_categories %>%
  mutate(
    Early_Quantile = case_when(
      Frequency_Early == 0 ~ "Q1",
      Frequency_Early > 0 & Frequency_Early <= early_non_zero_median ~ "Q2",
      Frequency_Early > early_non_zero_median ~ "Q3"
    ),
    Mid_Quantile = case_when(
      Frequency_Mid == 0 ~ "Q1",
      Frequency_Mid > 0 & Frequency_Mid <= mid_non_zero_median ~ "Q2",
      Frequency_Mid > mid_non_zero_median ~ "Q3"
    ),
    Late_Quantile = case_when(
      Frequency_Late == 0 ~ "Q1",
      Frequency_Late > 0 & Frequency_Late <= late_non_zero_median ~ "Q2",
      Frequency_Late > late_non_zero_median ~ "Q3"
    ),
    Total_Quantile = case_when(
      Total_Treatment_Frequency == 0 ~ "Q1",
      Total_Treatment_Frequency > 0 & Total_Treatment_Frequency <= total_non_zero_median ~ "Q2",
      Total_Treatment_Frequency > total_non_zero_median ~ "Q3"
    )
  )

# View the updated data frame
View(treatment_categories)


# Merge Injury_Frequency and Games_Missed_Frequency into treatment_categories
treatment_categories <- treatment_categories %>%
  left_join(
    injury_metrics %>% select(Player, Season, Injury_Frequency, Games_Missed_Frequency),
    by = c("Player", "Season")
  )













# FINAL: Export to csv
# Everything else (mean differences, propensity scores, msm) is done in rmd files

# Export treatment_categories to a CSV file
write.csv(treatment_categories, "treatment_categories.csv", row.names = FALSE)

# Export b2b_of_interest to a CSV file
write.csv(b2b_of_interest, "b2b_of_interest.csv", row.names = FALSE)

# Export all_game_logs_2 to a CSV file
write.csv(all_game_logs, "all_game_logs_2.csv", row.names = FALSE)




