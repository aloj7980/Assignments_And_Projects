# Load necessary libraries
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)


# Manually entered data for the Los Angeles Lakers' starters from 2013-2014 through 2022-2023
all_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Los Angeles Lakers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Dennis Schroder", "D'Angelo Russell", "Austin Reaves", "LeBron James", "Anthony Davis",
    "Russell Westbrook", "Avery Bradley", "Malik Monk", "LeBron James", "Anthony Davis",
    "Dennis Schroder", "Kentavious Caldwell-Pope", "LeBron James", "Anthony Davis", "Marc Gasol",
    "LeBron James", "Avery Bradley", "Kentavious Caldwell-Pope", "Anthony Davis", "JaVale McGee",
    "Lonzo Ball", "Brandon Ingram", "LeBron James", "Kyle Kuzma", "JaVale McGee",
    "Alex Caruso", "Lonzo Ball", "Josh Hart", "Brandon Ingram", "Kyle Kuzma",
    "Tyler Ennis", "Jordan Clarkson", "Brandon Ingram", "Julius Randle", "Larry Nance Jr.",
    "D'Angelo Russell", "Jordan Clarkson", "Kobe Bryant", "Julius Randle", "Roy Hibbert",
    "Jordan Clarkson", "Wayne Ellington", "Kobe Bryant", "Wesley Johnson", "Jordan Hill",
    "Kendall Marshall", "Jodie Meeks", "Wesley Johnson", "Jordan Hill", "Pau Gasol"
  ),
  stringsAsFactors = FALSE
)

# Add Atlanta Hawks data to the all_starters data frame
hawks_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Atlanta Hawks", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Trae Young", "Dejounte Murray", "De'Andre Hunter", "John Collins", "Clint Capela",
    "Trae Young", "Kevin Huerter", "De'Andre Hunter", "Danilo Gallinari", "John Collins",
    "Trae Young", "Bogdan Bogdanovic", "Kevin Huerter", "John Collins", "Clint Capela",
    "Trae Young", "Kevin Huerter", "De'Andre Hunter", "John Collins", "Dewayne Dedmon",
    "Trae Young", "Kevin Huerter", "Taurean Prince", "John Collins", "Dewayne Dedmon",
    "Dennis Schroder", "Kent Bazemore", "Taurean Prince", "Miles Plumlee", "Dewayne Dedmon",
    "Dennis Schroder", "Tim Hardaway Jr.", "Taurean Prince", "Paul Millsap", "Dwight Howard",
    "Jeff Teague", "Kyle Korver", "Kent Bazemore", "Paul Millsap", "Al Horford",
    "Jeff Teague", "Kyle Korver", "DeMarre Carroll", "Paul Millsap", "Al Horford",
    "Jeff Teague", "Kyle Korver", "DeMarre Carroll", "Paul Millsap", "Pero Antic"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, hawks_starters)

# Add Boston Celtics data to the all_starters data frame
celtics_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Boston Celtics", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Derrick White", "Jaylen Brown", "Jayson Tatum", "Al Horford", "Robert Williams",
    "Marcus Smart", "Jaylen Brown", "Jayson Tatum", "Al Horford", "Robert Williams",
    "Kemba Walker", "Marcus Smart", "Jaylen Brown", "Jayson Tatum", "Tristan Thompson",
    "Kemba Walker", "Jaylen Brown", "Gordon Hayward", "Jayson Tatum", "Daniel Theis",
    "Kyrie Irving", "Marcus Smart", "Jayson Tatum", "Marcus Morris", "Al Horford",
    "Kyrie Irving", "Jaylen Brown", "Jayson Tatum", "Al Horford", "Marcus Morris",
    "Marcus Smart", "Avery Bradley", "Jae Crowder", "Amir Johnson", "Al Horford",
    "Isaiah Thomas", "Jae Crowder", "Evan Turner", "Jonas Jerebko", "Amir Johnson",
    "Marcus Smart", "Avery Bradley", "Evan Turner", "Brandon Bass", "Tyler Zeller",
    "Rajon Rondo", "Avery Bradley", "Jeff Green", "Brandon Bass", "Jared Sullinger"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, celtics_starters)

# Add Brooklyn Nets data to the all_starters data frame
nets_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Brooklyn Nets", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Spencer Dinwiddie", "Mikal Bridges", "Dorian Finney-Smith", "Cameron Johnson", "Nicolas Claxton",
    "Kyrie Irving", "Seth Curry", "Bruce Brown Jr.", "Kevin Durant", "Andre Drummond",
    "Kyrie Irving", "James Harden", "Joe Harris", "Jeff Green", "DeAndre Jordan",
    "Spencer Dinwiddie", "Garrett Temple", "Joe Harris", "Taurean Prince", "Jarrett Allen",
    "D'Angelo Russell", "Joe Harris", "Caris LeVert", "Rodions Kurucs", "Jarrett Allen",
    "D'Angelo Russell", "Spencer Dinwiddie", "Allen Crabbe", "Rondae Hollis-Jefferson", "DeMarre Carroll",
    "Jeremy Lin", "Randy Foye", "Caris LeVert", "Rondae Hollis-Jefferson", "Justin Hamilton",
    "Donald Sloan", "Wayne Ellington", "Bojan Bogdanovic", "Thaddeus Young", "Brook Lopez",
    "Deron Williams", "Joe Johnson", "Bojan Bogdanovic", "Thaddeus Young", "Brook Lopez",
    "Deron Williams", "Shaun Livingston", "Joe Johnson", "Paul Pierce", "Kevin Garnett"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, nets_starters)

# Add Charlotte Hornets data to the all_starters data frame
hornets_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Charlotte Hornets", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "LaMelo Ball", "Terry Rozier", "Kelly Oubre Jr.", "Gordon Hayward", "P.J. Washington",
    "LaMelo Ball", "Terry Rozier", "Gordon Hayward", "Miles Bridges", "Mason Plumlee",
    "Terry Rozier", "Devonte' Graham", "Gordon Hayward", "P.J. Washington", "Bismack Biyombo",
    "Terry Rozier", "Devonte' Graham", "Miles Bridges", "P.J. Washington", "Cody Zeller",
    "Kemba Walker", "Jeremy Lamb", "Nicolas Batum", "Marvin Williams", "Cody Zeller",
    "Kemba Walker", "Nicolas Batum", "Michael Kidd-Gilchrist", "Marvin Williams", "Dwight Howard",
    "Kemba Walker", "Nicolas Batum", "Michael Kidd-Gilchrist", "Marvin Williams", "Cody Zeller",
    "Kemba Walker", "Courtney Lee", "Marvin Williams", "Frank Kaminsky", "Al Jefferson",
    "Kemba Walker", "Gerald Henderson", "Michael Kidd-Gilchrist", "Cody Zeller", "Al Jefferson",
    "Kemba Walker", "Gerald Henderson", "Michael Kidd-Gilchrist", "Josh McRoberts", "Al Jefferson"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, hornets_starters)

# Add Chicago Bulls data to the all_starters data frame
bulls_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Chicago Bulls", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Patrick Beverley", "Zach LaVine", "DeMar DeRozan", "Patrick Williams", "Nikola Vucevic",
    "Alex Caruso", "Zach LaVine", "DeMar DeRozan", "Patrick Williams", "Nikola Vucevic",
    "Coby White", "Zach LaVine", "Patrick Williams", "Daniel Theis", "Nikola Vucevic",
    "Tomas Satoransky", "Zach LaVine", "Kris Dunn", "Lauri Markkanen", "Wendell Carter Jr.",
    "Kris Dunn", "Zach LaVine", "Lauri Markkanen", "Wendell Carter Jr.", "Robin Lopez",
    "Kris Dunn", "Justin Holiday", "Denzel Valentine", "Lauri Markkanen", "Robin Lopez",
    "Isaiah Canaan", "Jerian Grant", "Dwyane Wade", "Nikola Mirotic", "Robin Lopez",
    "Derrick Rose", "Jimmy Butler", "Nikola Mirotic", "Taj Gibson", "Pau Gasol",
    "Derrick Rose", "Jimmy Butler", "Mike Dunleavy", "Pau Gasol", "Joakim Noah",
    "Kirk Hinrich", "Jimmy Butler", "Mike Dunleavy", "Carlos Boozer", "Joakim Noah"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, bulls_starters)

# Add Cleveland Cavaliers data to the all_starters data frame
cavaliers_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Cleveland Cavaliers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Darius Garland", "Donovan Mitchell", "Caris LeVert", "Evan Mobley", "Jarrett Allen",
    "Darius Garland", "Isaac Okoro", "Lauri Markkanen", "Evan Mobley", "Jarrett Allen",
    "Darius Garland", "Collin Sexton", "Isaac Okoro", "Larry Nance Jr.", "Jarrett Allen",
    "Darius Garland", "Collin Sexton", "Cedi Osman", "Kevin Love", "Andre Drummond",
    "Collin Sexton", "Brandon Knight", "Cedi Osman", "Tristan Thompson", "Larry Nance Jr.",
    "J.R. Smith", "Kyle Korver", "Cedi Osman", "Tristan Thompson", "Kevin Love",
    "Kyrie Irving", "J.R. Smith", "LeBron James", "Kevin Love", "Tristan Thompson",
    "Kyrie Irving", "J.R. Smith", "LeBron James", "Kevin Love", "Tristan Thompson",
    "Kyrie Irving", "Iman Shumpert", "LeBron James", "Tristan Thompson", "Timofey Mozgov",
    "Kyrie Irving", "Jarrett Jack", "C.J. Miles", "Tristan Thompson", "Anderson Varejao"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, cavaliers_starters)

# Add Dallas Mavericks data to the all_starters data frame
mavericks_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Dallas Mavericks", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Luka Doncic", "Kyrie Irving", "Tim Hardaway Jr.", "Reggie Bullock", "Dwight Powell",
    "Luka Doncic", "Jalen Brunson", "Reggie Bullock", "Dorian Finney-Smith", "Dwight Powell",
    "Luka Doncic", "Josh Richardson", "Dorian Finney-Smith", "Maxi Kleber", "Kristaps Porzingis",
    "Luka Doncic", "Tim Hardaway Jr.", "Dorian Finney-Smith", "Dwight Powell", "Kristaps Porzingis",
    "Luka Doncic", "Jalen Brunson", "Dorian Finney-Smith", "Dirk Nowitzki", "Dwight Powell",
    "Dennis Smith", "Wesley Matthews", "Harrison Barnes", "Maxi Kleber", "Dwight Powell",
    "Yogi Ferrell", "Wesley Matthews", "Dorian Finney-Smith", "Dirk Nowitzki", "Nerlens Noel",
    "Raymond Felton", "Deron Williams", "Wesley Matthews", "Dirk Nowitzki", "Zaza Pachulia",
    "Rajon Rondo", "Monta Ellis", "Chandler Parsons", "Dirk Nowitzki", "Tyson Chandler",
    "Jose Calderon", "Monta Ellis", "Shawn Marion", "Dirk Nowitzki", "Samuel Dalembert"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, mavericks_starters)

# Add Denver Nuggets data to the all_starters data frame
nuggets_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Denver Nuggets", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Jamal Murray", "Kentavious Caldwell-Pope", "Michael Porter Jr.", "Aaron Gordon", "Nikola Jokic",
    "Monte Morris", "Will Barton", "Aaron Gordon", "Jeff Green", "Nikola Jokic",
    "Jamal Murray", "Will Barton", "Michael Porter Jr.", "Aaron Gordon", "Nikola Jokic",
    "Jamal Murray", "Gary Harris", "Will Barton", "Paul Millsap", "Nikola Jokic",
    "Jamal Murray", "Gary Harris", "Will Barton", "Paul Millsap", "Nikola Jokic",
    "Jamal Murray", "Gary Harris", "Wilson Chandler", "Paul Millsap", "Mason Plumlee",
    "Jamal Murray", "Gary Harris", "Danilo Gallinari", "Wilson Chandler", "Nikola Jokic",
    "Emmanuel Mudiay", "Gary Harris", "Danilo Gallinari", "Kenneth Faried", "Nikola Jokic",
    "Ty Lawson", "Danilo Gallinari", "Wilson Chandler", "Kenneth Faried", "Jusuf Nurkic",
    "Ty Lawson", "Randy Foye", "Wilson Chandler", "Kenneth Faried", "J.J. Hickson"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, nuggets_starters)

# Add Detroit Pistons data to the all_starters data frame
pistons_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Detroit Pistons", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Killian Hayes", "Jaden Ivey", "Bojan Bogdanovic", "Jalen Duren", "Isaiah Stewart II",
    "Killian Hayes", "Cade Cunningham", "Saddiq Bey", "Jerami Grant", "Isaiah Stewart II",
    "Wayne Ellington", "Josh Jackson", "Saddiq Bey", "Jerami Grant", "Mason Plumlee",
    "Bruce Brown Jr.", "Luke Kennard", "Tony Snell", "Sekou Doumbouya", "Svi Mykhailiuk",
    "Reggie Jackson", "Wayne Ellington", "Bruce Brown Jr.", "Blake Griffin", "Andre Drummond",
    "Ish Smith", "Reggie Jackson", "Reggie Bullock", "Stanley Johnson", "Andre Drummond",
    "Ish Smith", "Kentavious Caldwell-Pope", "Marcus Morris", "Jon Leuer", "Andre Drummond",
    "Reggie Jackson", "Kentavious Caldwell-Pope", "Marcus Morris", "Tobias Harris", "Andre Drummond",
    "Brandon Jennings", "Kentavious Caldwell-Pope", "Caron Butler", "Greg Monroe", "Andre Drummond",
    "Brandon Jennings", "Kentavious Caldwell-Pope", "Josh Smith", "Greg Monroe", "Andre Drummond"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, pistons_starters)

# Add Golden State Warriors data to the all_starters data frame
warriors_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Golden State Warriors", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Stephen Curry", "Gary Payton II", "Klay Thompson", "Andrew Wiggins", "Draymond Green",
    "Stephen Curry", "Klay Thompson", "Andrew Wiggins", "Otto Porter", "Draymond Green",
    "Stephen Curry", "Andrew Wiggins", "Kelly Oubre, Jr.", "Draymond Green", "Kevon Looney",
    "Damion Lee", "Andrew Wiggins", "Draymond Green", "Eric Paschall", "Marquese Chriss",
    "Stephen Curry", "Klay Thompson", "Kevin Durant", "Draymond Green", "DeMarcus Cousins",
    "Quinn Cook", "Stephen Curry", "Klay Thompson", "Draymond Green", "Jordan Bell",
    "Stephen Curry", "Klay Thompson", "Kevin Durant", "Draymond Green", "Zaza Pachulia",
    "Stephen Curry", "Klay Thompson", "Harrison Barnes", "Draymond Green", "Andrew Bogut",
    "Stephen Curry", "Klay Thompson", "Harrison Barnes", "Draymond Green", "Andrew Bogut",
    "Stephen Curry", "Klay Thompson", "Andre Iguodala", "Draymond Green", "David Lee"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, warriors_starters)

# Add Houston Rockets data to the all_starters data frame
rockets_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Houston Rockets", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Kevin Porter Jr.", "Jalen Green", "Kenyon Martin Jr.", "Jabari Smith Jr.", "Alperen Sengun",
    "Kevin Porter Jr.", "Jalen Green", "Eric Gordon", "Garrison Mathews", "Jae'Sean Tate",
    "Kevin Porter Jr.", "John Wall", "Jae'Sean Tate", "Kelly Olynyk", "Christian Wood",
    "Russell Westbrook", "James Harden", "Danuel House", "P.J. Tucker", "Robert Covington",
    "Chris Paul", "Eric Gordon", "James Harden", "P.J. Tucker", "Clint Capela",
    "Eric Gordon", "James Harden", "P.J. Tucker", "Ryan Anderson", "Nene Hilario",
    "Patrick Beverley", "James Harden", "Trevor Ariza", "Ryan Anderson", "Clint Capela",
    "Jason Terry", "James Harden", "Trevor Ariza", "Terrence Jones", "Dwight Howard",
    "Patrick Beverley", "James Harden", "Trevor Ariza", "Donatas Motiejunas", "Dwight Howard",
    "Jeremy Lin", "James Harden", "Francisco Garcia", "Terrence Jones", "Omer Asik"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, rockets_starters)

# Add Indiana Pacers data to the all_starters data frame
pacers_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Indiana Pacers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Tyrese Haliburton", "Andrew Nembhard", "Buddy Hield", "Aaron Nesmith", "Myles Turner",
    "Malcolm Brogdon", "Buddy Hield", "Tyrese Haliburton", "Chris Duarte", "Myles Turner",
    "Malcolm Brogdon", "Caris LeVert", "Justin Holiday", "Domantas Sabonis", "Myles Turner",
    "Malcolm Brogdon", "Jeremy Lamb", "T.J. Warren", "Domantas Sabonis", "Myles Turner",
    "Darren Collison", "Victor Oladipo", "Bojan Bogdanovic", "Thaddeus Young", "Myles Turner",
    "Darren Collison", "Victor Oladipo", "Bojan Bogdanovic", "Thaddeus Young", "Myles Turner",
    "Jeff Teague", "Monta Ellis", "Paul George", "Thaddeus Young", "Myles Turner",
    "George Hill", "Monta Ellis", "Paul George", "Myles Turner", "Ian Mahinmi",
    "George Hill", "C.J. Miles", "Solomon Hill", "David West", "Roy Hibbert",
    "George Hill", "Lance Stephenson", "Paul George", "David West", "Roy Hibbert"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, pacers_starters)

# Add Los Angeles Clippers data to the all_starters data frame
clippers_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Los Angeles Clippers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Russell Westbrook", "Eric Gordon", "Norman Powell", "Nicolas Batum", "Ivica Zubac",
    "Reggie Jackson", "Nicolas Batum", "Robert Covington", "Marcus Morris", "Ivica Zubac",
    "Reggie Jackson", "Paul George", "Nicolas Batum", "Kawhi Leonard", "Serge Ibaka",
    "Patrick Beverley", "Paul George", "Kawhi Leonard", "Marcus Morris", "Ivica Zubac",
    "Patrick Beverley", "Shai Gilgeous-Alexander", "Landry Shamet", "Danilo Gallinari", "Ivica Zubac",
    "Milos Teodosic", "Lou Williams", "C.J. Williams", "Danilo Gallinari", "Wesley Johnson",
    "J.J. Redick", "Austin Rivers", "Luc Mbah a Moute", "Marreese Speights", "DeAndre Jordan",
    "Chris Paul", "Austin Rivers", "Luc Mbah a Moute", "Blake Griffin", "DeAndre Jordan",
    "Chris Paul", "J.J. Redick", "Matt Barnes", "Blake Griffin", "DeAndre Jordan",
    "Chris Paul", "J.J. Redick", "Matt Barnes", "Blake Griffin", "DeAndre Jordan"
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, clippers_starters)

# Add Memphis Grizzlies data to the all_starters data frame
grizzlies_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Memphis Grizzlies", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Ja Morant", "Desmond Bane", "Dillon Brooks", "Jaren Jackson, Jr.", "Xavier Tillman, Sr.",
    "Tyus Jones", "Desmond Bane", "Dillon Brooks", "Jaren Jackson, Jr.", "Steven Adams",
    "Ja Morant", "Grayson Allen", "Dillon Brooks", "Kyle Anderson", "Jonas Valanciunas",
    "Ja Morant", "Dillon Brooks", "Jaren Jackson, Jr.", "Kyle Anderson", "Jonas Valanciunas",
    "Mike Conley", "Justin Holiday", "Kyle Anderson", "Jaren Jackson, Jr.", "Bruno Caboclo",
    "Andrew Harrison", "Dillon Brooks", "Jarell Martin", "JaMychal Green", "Marc Gasol",
    "Mike Conley", "Vince Carter", "James Ennis", "Zach Randolph", "Marc Gasol",
    "Jordan Farmar", "Tony Allen", "Vince Carter", "Matt Barnes", "Zach Randolph",
    "Mike Conley", "Courtney Lee", "Tony Allen", "Zach Randolph", "Marc Gasol",
    "Mike Conley", "Courtney Lee", "Tony Allen", "Zach Randolph", "Marc Gasol"
    
  ),
  stringsAsFactors = FALSE
)

# Append the new data to the existing all_starters data frame
all_starters <- bind_rows(all_starters, grizzlies_starters)

# Add Miami Heat data to the all_starters data frame
heat_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Miami Heat", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Gabe Vincent", "Max Strus", "Jimmy Butler", "Kevin Love", "Bam Adebayo",
    "Kyle Lowry", "Max Strus", "Jimmy Butler", "P.J. Tucker", "Bam Adebayo",
    "Kendrick Nunn", "Victor Oladipo", "Duncan Robinson", "Jimmy Butler", "Bam Adebayo",
    "Kendrick Nunn", "Jimmy Butler", "Jae Crowder", "Duncan Robinson", "Bam Adebayo",
    "Justise Winslow", "Josh Richardson", "James Johnson", "Kelly Olynyk", "Hassan Whiteside",
    "Goran Dragic", "Tyler Johnson", "Josh Richardson", "James Johnson", "Hassan Whiteside",
    "Goran Dragic", "Josh Richardson", "Rodney McGruder", "James Johnson", "Hassan Whiteside",
    "Goran Dragic", "Dwyane Wade", "Joe Johnson", "Luol Deng", "Hassan Whiteside",
    "Mario Chalmers", "Dwyane Wade", "Luol Deng", "Chris Bosh", "Hassan Whiteside",
    "Mario Chalmers", "Dwyane Wade", "LeBron James", "Shane Battier", "Chris Bosh"
  ),
  stringsAsFactors = FALSE
)

# Add Milwaukee Bucks data to the all_starters data frame
bucks_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Milwaukee Bucks", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Jrue Holiday", "Grayson Allen", "Khris Middleton", "Giannis Antetokounmpo", "Brook Lopez",
    "Jrue Holiday", "Grayson Allen", "Wesley Matthews", "Giannis Antetokounmpo", "Brook Lopez",
    "Jrue Holiday", "Donte DiVincenzo", "Khris Middleton", "Giannis Antetokounmpo", "Brook Lopez",
    "Eric Bledsoe", "Wesley Matthews", "Khris Middleton", "Giannis Antetokounmpo", "Brook Lopez",
    "Eric Bledsoe", "Malcolm Brogdon", "Khris Middleton", "Giannis Antetokounmpo", "Brook Lopez",
    "Malcolm Brogdon", "Tony Snell", "Khris Middleton", "Giannis Antetokounmpo", "John Henson",
    "Malcolm Brogdon", "Tony Snell", "Khris Middleton", "Giannis Antetokounmpo", "Thon Maker",
    "Michael Carter-Williams", "Khris Middleton", "Giannis Antetokounmpo", "Jabari Parker", "Greg Monroe",
    "Michael Carter-Williams", "Khris Middleton", "Giannis Antetokounmpo", "Ersan Ilyasova", "Zaza Pachulia",
    "Brandon Knight", "O.J. Mayo", "Khris Middleton", "Ersan Ilyasova", "Larry Sanders"
  ),
  stringsAsFactors = FALSE
)

# Add Minnesota Timberwolves data to the all_starters data frame
timberwolves_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Minnesota Timberwolves", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Mike Conley", "Anthony Edwards", "Nickeil Alexander-Walker", "Karl-Anthony Towns", "Rudy Gobert",
    "Patrick Beverley", "D'Angelo Russell", "Anthony Edwards", "Jarred Vanderbilt", "Karl-Anthony Towns",
    "Ricky Rubio", "Malik Beasley", "Josh Okogie", "Anthony Edwards", "Karl-Anthony Towns",
    "D'Angelo Russell", "Malik Beasley", "Jarrett Culver", "Josh Okogie", "Karl-Anthony Towns",
    "Jeff Teague", "Josh Okogie", "Andrew Wiggins", "Taj Gibson", "Karl-Anthony Towns",
    "Jeff Teague", "Jimmy Butler", "Andrew Wiggins", "Taj Gibson", "Karl-Anthony Towns",
    "Ricky Rubio", "Brandon Rush", "Andrew Wiggins", "Gorgui Dieng", "Karl-Anthony Towns",
    "Ricky Rubio", "Andrew Wiggins", "Tayshaun Prince", "Gorgui Dieng", "Karl-Anthony Towns",
    "Zach LaVine", "Kevin Martin", "Andrew Wiggins", "Gorgui Dieng", "Nikola Pekovic",
    "Ricky Rubio", "Kevin Martin", "Corey Brewer", "Kevin Love", "Nikola Pekovic"
  ),
  stringsAsFactors = FALSE
)

# Add New Orleans Pelicans data to the all_starters data frame
pelicans_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("New Orleans Pelicans", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "C.J. McCollum", "Herb Jones", "Brandon Ingram", "Trey Murphy III", "Jonas Valanciunas",
    "C.J. McCollum", "Herb Jones", "Brandon Ingram", "Jaxson Hayes", "Jonas Valanciunas",
    "Lonzo Ball", "Eric Bledsoe", "Brandon Ingram", "Zion Williamson", "Steven Adams",
    "Lonzo Ball", "J.J. Redick", "Jrue Holiday", "Brandon Ingram", "Derrick Favors",
    "Elfrid Payton", "E'Twaun Moore", "Jrue Holiday", "Julius Randle", "Anthony Davis",
    "Jrue Holiday", "E'Twaun Moore", "Darius Miller", "Anthony Davis", "Emeka Okafor",
    "Jrue Holiday", "Solomon Hill", "Anthony Davis", "Dante Cunningham", "Alexis Ajinca",
    "Eric Gordon", "Alonzo Gee", "Dante Cunningham", "Anthony Davis", "Omer Asik",
    "Tyreke Evans", "Eric Gordon", "Quincy Pondexter", "Anthony Davis", "Omer Asik",
    "Brian Roberts", "Jrue Holiday", "Eric Gordon", "Al-Farouq Aminu", "Anthony Davis"
  ),
  stringsAsFactors = FALSE
)

# Add New York Knicks data to the all_starters data frame
knicks_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("New York Knicks", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Jalen Brunson", "Quentin Grimes", "R.J. Barrett", "Julius Randle", "Mitchell Robinson",
    "Alec Burks", "Evan Fournier", "R.J. Barrett", "Julius Randle", "Mitchell Robinson",
    "Elfrid Payton", "R.J. Barrett", "Reggie Bullock", "Julius Randle", "Nerlens Noel",
    "Frank Ntilikina", "R.J. Barrett", "Reggie Bullock", "Maurice Harkless", "Julius Randle",
    "Emmanuel Mudiay", "Damyean Dotson", "Mario Hezonja", "Kevin Knox", "Noah Vonleh",
    "Courtney Lee", "Tim Hardaway Jr.", "Lance Thomas", "Kristaps Porzingis", "Enes Kanter",
    "Ron Baker", "Courtney Lee", "Carmelo Anthony", "Maurice Ndour", "Willy Hernangomez",
    "Jose Calderon", "Arron Afflalo", "Carmelo Anthony", "Kristaps Porzingis", "Robin Lopez",
    "Jose Calderon", "Langston Galloway", "Tim Hardaway Jr.", "Carmelo Anthony", "Jason Smith",
    "Raymond Felton", "Iman Shumpert", "Carmelo Anthony", "Andrea Bargnani", "Tyson Chandler"
  ),
  stringsAsFactors = FALSE
)

# Add Oklahoma City Thunder data to the all_starters data frame
thunder_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Oklahoma City Thunder", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "Josh Giddey", "Shai Gilgeous-Alexander", "Luguentz Dort", "Jalen Williams", "Jaylin Williams",
    # 2021-2022 season
    "Shai Gilgeous-Alexander", "Luguentz Dort", "Josh Giddey", "Darius Bazley", "Jeremiah Robinson-Earl",
    # 2020-2021 season
    "Theo Maledon", "Shai Gilgeous-Alexander", "Luguentz Dort", "Darius Bazley", "Al Horford",
    # 2019-2020 season
    "Chris Paul", "Shai Gilgeous-Alexander", "Terrance Ferguson", "Danilo Gallinari", "Steven Adams",
    # 2018-2019 season
    "Russell Westbrook", "Terrance Ferguson", "Paul George", "Jerami Grant", "Steven Adams",
    # 2017-2018 season
    "Russell Westbrook", "Andre Roberson", "Paul George", "Carmelo Anthony", "Steven Adams",
    # 2016-2017 season
    "Russell Westbrook", "Victor Oladipo", "Andre Roberson", "Taj Gibson", "Steven Adams",
    # 2015-2016 season
    "Russell Westbrook", "Andre Roberson", "Kevin Durant", "Serge Ibaka", "Steven Adams",
    # 2014-2015 season
    "Russell Westbrook", "Andre Roberson", "Kevin Durant", "Serge Ibaka", "Steven Adams",
    # 2013-2014 season
    "Russell Westbrook", "Thabo Sefolosha", "Kevin Durant", "Serge Ibaka", "Kendrick Perkins"
  ),
  stringsAsFactors = FALSE
)

# Add Orlando Magic data to the all_starters data frame
magic_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Orlando Magic", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "Markelle Fultz", "Gary Harris", "Franz Wagner", "Paolo Banchero", "Wendell Carter, Jr.",
    # 2021-2022 season
    "Cole Anthony", "Jalen Suggs", "Franz Wagner", "Wendell Carter, Jr.", "Mohamed Bamba",
    # 2020-2021 season
    "Cole Anthony", "Gary Harris", "James Ennis", "Dwayne Bacon", "Wendell Carter, Jr.",
    # 2019-2020 season
    "Markelle Fultz", "Evan Fournier", "Aaron Gordon", "Jonathan Isaac", "Nikola Vucevic",
    # 2018-2019 season
    "D.J. Augustin", "Evan Fournier", "Aaron Gordon", "Jonathan Isaac", "Nikola Vucevic",
    # 2017-2018 season
    "D.J. Augustin", "Evan Fournier", "Jonathon Simmons", "Aaron Gordon", "Nikola Vucevic",
    # 2016-2017 season
    "Elfrid Payton", "Evan Fournier", "Terrence Ross", "Aaron Gordon", "Nikola Vucevic",
    # 2015-2016 season
    "Elfrid Payton", "Victor Oladipo", "Evan Fournier", "Aaron Gordon", "Nikola Vucevic",
    # 2014-2015 season
    "Elfrid Payton", "Victor Oladipo", "Tobias Harris", "Channing Frye", "Nikola Vucevic",
    # 2013-2014 season
    "Jameer Nelson", "Victor Oladipo", "Arron Afflalo", "Maurice Harkless", "Nikola Vucevic"
  ),
  stringsAsFactors = FALSE
)

# Add Philadelphia 76ers data to the all_starters data frame
sixers_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Philadelphia 76ers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "James Harden", "Tyrese Maxey", "Tobias Harris", "P.J. Tucker", "Joel Embiid",
    "James Harden", "Tyrese Maxey", "Tobias Harris", "DeAndre Jordan", "Joel Embiid",
    "Ben Simmons", "Seth Curry", "Danny Green", "Tobias Harris", "Joel Embiid",
    "Ben Simmons", "Glenn Robinson", "Tobias Harris", "Al Horford", "Joel Embiid",
    "Ben Simmons", "J.J. Redick", "Jimmy Butler", "Tobias Harris", "Joel Embiid",
    "Ben Simmons", "Jerryd Bayless", "Dario Saric", "Robert Covington", "Joel Embiid",
    "T.J. McConnell", "Timothe Luwawu-Cabarrot", "Justin Anderson", "Dario Saric", "Richaun Holmes",
    "Isaiah Canaan", "Jerami Grant", "Nerlens Noel", "Robert Covington", "Jahlil Okafor",
    "JaKarr Sampson", "Robert Covington", "Luc Mbah a Moute", "Henry Sims", "Nerlens Noel",
    "Michael Carter-Williams", "Tony Wroten", "James Anderson", "Hollis Thompson", "Thaddeus Young"
  ),
  stringsAsFactors = FALSE
)

# Add Phoenix Suns data to the all_starters data frame
suns_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Phoenix Suns", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Chris Paul", "Devin Booker", "Josh Okogie", "Kevin Durant", "Deandre Ayton",
    "Chris Paul", "Devin Booker", "Mikal Bridges", "Jae Crowder", "Deandre Ayton",
    "Chris Paul", "Devin Booker", "Mikal Bridges", "Jae Crowder", "Deandre Ayton",
    "Ricky Rubio", "Devin Booker", "Mikal Bridges", "Dario Saric", "Deandre Ayton",
    "De'Anthony Melton", "Devin Booker", "Mikal Bridges", "T.J. Warren", "Deandre Ayton",
    "Devin Booker", "T.J. Warren", "Marquese Chriss", "Dragan Bender", "Tyson Chandler",
    "Tyler Ulis", "Devin Booker", "T.J. Warren", "Marquese Chriss", "Alex Len",
    "Brandon Knight", "Devin Booker", "P.J. Tucker", "Tyson Chandler", "Alex Len",
    "Eric Bledsoe", "P.J. Tucker", "Marcus Morris", "Markieff Morris", "Alex Len",
    "Goran Dragic", "Gerald Green", "P.J. Tucker", "Channing Frye", "Miles Plumlee"
  ),
  stringsAsFactors = FALSE
)

# Add Portland Trail Blazers data to the all_starters data frame
trailblazers_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Portland Trail Blazers", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "Damian Lillard", "Anfernee Simons", "Jerami Grant", "Drew Eubanks", "Jusuf Nurkic",
    "Damian Lillard", "Anfernee Simons", "Norman Powell", "Josh Hart", "Jusuf Nurkic",
    "Damian Lillard", "C.J. McCollum", "Norman Powell", "Derrick Jones", "Robert Covington",
    "Damian Lillard", "C.J. McCollum", "Rodney Hood", "Carmelo Anthony", "Hassan Whiteside",
    "Damian Lillard", "C.J. McCollum", "Maurice Harkless", "Al-Farouq Aminu", "Jusuf Nurkic",
    "Damian Lillard", "C.J. McCollum", "Evan Turner", "Maurice Harkless", "Al-Farouq Aminu",
    "Damian Lillard", "C.J. McCollum", "Evan Turner", "Maurice Harkless", "Noah Vonleh",
    "Damian Lillard", "C.J. McCollum", "Al-Farouq Aminu", "Moe Harkless", "Mason Plumlee",
    "Damian Lillard", "Arron Afflalo", "Nicolas Batum", "LaMarcus Aldridge", "Robin Lopez",
    "Damian Lillard", "Wesley Matthews", "Nicolas Batum", "LaMarcus Aldridge", "Robin Lopez"
  ),
  stringsAsFactors = FALSE
)

# Add Sacramento Kings data to the all_starters data frame
kings_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Sacramento Kings", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "De'Aaron Fox", "Kevin Huerter", "Harrison Barnes", "Keegan Murray", "Domantas Sabonis",
    # 2021-2022 season
    "De'Aaron Fox", "Justin Holiday", "Harrison Barnes", "Domantas Sabonis", "Richaun Holmes",
    # 2020-2021 season
    "De'Aaron Fox", "Buddy Hield", "Harrison Barnes", "Marvin Bagley III", "Richaun Holmes",
    # 2019-2020 season
    "De'Aaron Fox", "Buddy Hield", "Harrison Barnes", "Nemanja Bjelica", "Richaun Holmes",
    # 2018-2019 season
    "De'Aaron Fox", "Buddy Hield", "Harrison Barnes", "Nemanja Bjelica", "Willie Cauley-Stein",
    # 2017-2018 season
    "De'Aaron Fox", "Bogdan Bogdanovic", "Justin Jackson", "Zach Randolph", "Willie Cauley-Stein",
    # 2016-2017 season
    "Ty Lawson", "Langston Galloway", "Buddy Hield", "Skal Labissiere", "Willie Cauley-Stein",
    # 2015-2016 season
    "Rajon Rondo", "Ben McLemore", "Rudy Gay", "DeMarcus Cousins", "Willie Cauley-Stein",
    # 2014-2015 season
    "Darren Collison", "Ben McLemore", "Rudy Gay", "Jason Thompson", "DeMarcus Cousins",
    # 2013-2014 season
    "Isaiah Thomas", "Ray McCallum", "Ben McLemore", "Jason Thompson", "DeMarcus Cousins"
  ),
  stringsAsFactors = FALSE
)

# Add San Antonio Spurs data to the all_starters data frame
spurs_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("San Antonio Spurs", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "Tre Jones", "Devin Vassell", "Keldon Johnson", "Jeremy Sochan", "Keita Bates-Diop",
    # 2021-2022 season
    "Dejounte Murray", "Keldon Johnson", "Devin Vassell", "Doug McDermott", "Jakob Poeltl",
    # 2020-2021 season
    "Dejounte Murray", "Lonnie Walker", "Keldon Johnson", "DeMar DeRozan", "Jakob Poeltl",
    # 2019-2020 season
    "Dejounte Murray", "Bryn Forbes", "DeMar DeRozan", "LaMarcus Aldridge", "Trey Lyles",
    # 2018-2019 season
    "Bryn Forbes", "Derrick White", "DeMar DeRozan", "Rudy Gay", "LaMarcus Aldridge",
    # 2017-2018 season
    "Patty Mills", "Dejounte Murray", "Danny Green", "LaMarcus Aldridge", "Pau Gasol",
    # 2016-2017 season
    "Patty Mills", "Jonathon Simmons", "Danny Green", "LaMarcus Aldridge", "Pau Gasol",
    # 2015-2016 season
    "Tony Parker", "Danny Green", "Kawhi Leonard", "Tim Duncan", "LaMarcus Aldridge",
    # 2014-2015 season
    "Tony Parker", "Danny Green", "Kawhi Leonard", "Tim Duncan", "Tiago Splitter",
    # 2013-2014 season
    "Tony Parker", "Danny Green", "Kawhi Leonard", "Tim Duncan", "Tiago Splitter"
  ),
  stringsAsFactors = FALSE
)

# Add Toronto Raptors data to the all_starters data frame
raptors_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Toronto Raptors", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "Fred VanVleet", "Scottie Barnes", "OG Anunoby", "Pascal Siakam", "Jakob Poeltl",
    # 2021-2022 season
    "Fred VanVleet", "Gary Trent Jr.", "OG Anunoby", "Pascal Siakam", "Khem Birch",
    # 2020-2021 season
    "Kyle Lowry", "Fred VanVleet", "Gary Trent Jr.", "OG Anunoby", "Pascal Siakam",
    # 2019-2020 season
    "Fred VanVleet", "Kyle Lowry", "OG Anunoby", "Pascal Siakam", "Marc Gasol",
    # 2018-2019 season
    "Kyle Lowry", "Danny Green", "Kawhi Leonard", "Pascal Siakam", "Serge Ibaka",
    # 2017-2018 season
    "Kyle Lowry", "DeMar DeRozan", "OG Anunoby", "Serge Ibaka", "Jonas Valanciunas",
    # 2016-2017 season
    "Kyle Lowry", "DeMar DeRozan", "Norman Powell", "Serge Ibaka", "Jonas Valanciunas",
    # 2015-2016 season
    "Kyle Lowry", "DeMar DeRozan", "DeMarre Carroll", "Bismack Biyombo", "Jonas Valanciunas",
    # 2014-2015 season
    "Kyle Lowry", "DeMar DeRozan", "Terrence Ross", "Amir Johnson", "Jonas Valanciunas",
    # 2013-2014 season
    "Kyle Lowry", "DeMar DeRozan", "Terrence Ross", "Amir Johnson", "Jonas Valanciunas"
  ),
  stringsAsFactors = FALSE
)

# Add Utah Jazz data to the all_starters data frame
jazz_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Utah Jazz", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    "J. Clarkson", "Ochai Agbaji", "L. Markkanen", "Kelly Olynyk", "Walker Kessler",
    "Mike Conley", "D. Mitchell", "B. Bogdanovic", "Royce O'Neale", "Rudy Gobert",
    "Mike Conley", "D. Mitchell", "B. Bogdanovic", "Royce O'Neale", "Rudy Gobert",
    "D. Mitchell", "Joe Ingles", "B. Bogdanovic", "Royce O'Neale", "Rudy Gobert",
    "Ricky Rubio", "D. Mitchell", "Joe Ingles", "Derrick Favors", "Rudy Gobert",
    "Ricky Rubio", "D. Mitchell", "Joe Ingles", "Jonas Jerebko", "Rudy Gobert",
    "Shelvin Mack", "Joe Ingles", "Gordon Hayward", "Boris Diaw", "Rudy Gobert",
    "Raulzinho Neto", "Rodney Hood", "Gordon Hayward", "Derrick Favors", "Rudy Gobert",
    "Trey Burke", "Dante Exum", "Gordon Hayward", "Derrick Favors", "Rudy Gobert",
    "Trey Burke", "Gordon Hayward", "R. Jefferson", "M. Williams", "Derrick Favors"
  ),
  stringsAsFactors = FALSE
)

# Add Washington Wizards data to the all_starters data frame
wizards_starters <- data.frame(
  Season = c(
    rep("2022-2023", 5), rep("2021-2022", 5), rep("2020-2021", 5),
    rep("2019-2020", 5), rep("2018-2019", 5), rep("2017-2018", 5),
    rep("2016-2017", 5), rep("2015-2016", 5), rep("2014-2015", 5),
    rep("2013-2014", 5)
  ),
  Team = rep("Washington Wizards", 50),
  Position = rep(c("PG", "SG", "SF", "PF", "C"), 10),
  Player = c(
    # 2022-2023 season
    "Monte Morris", "Bradley Beal", "Corey Kispert", "Kyle Kuzma", "Daniel Gafford",
    # 2021-2022 season
    "Bradley Beal", "Kentavious Caldwell-Pope", "Kyle Kuzma", "Kristaps Porzingis", "Daniel Gafford",
    # 2020-2021 season
    "Russell Westbrook", "Bradley Beal", "Deni Avdija", "Rui Hachimura", "Alex Len",
    # 2019-2020 season
    "Bradley Beal", "Isaac Bonga", "Rui Hachimura", "Thomas Bryant", "Ian Mahinmi",
    # 2018-2019 season
    "Tomas Satoransky", "Bradley Beal", "Trevor Ariza", "Jeff Green", "Thomas Bryant",
    # 2017-2018 season
    "John Wall", "Tomas Satoransky", "Bradley Beal", "Otto Porter", "Marcin Gortat",
    # 2016-2017 season
    "John Wall", "Bradley Beal", "Otto Porter", "Markieff Morris", "Marcin Gortat",
    # 2015-2016 season
    "John Wall", "Garrett Temple", "Jared Dudley", "Otto Porter", "Marcin Gortat",
    # 2014-2015 season
    "John Wall", "Bradley Beal", "Paul Pierce", "Nene Hilario", "Marcin Gortat",
    # 2013-2014 season
    "John Wall", "Bradley Beal", "Trevor Ariza", "Nene Hilario", "Marcin Gortat"
  ),
  stringsAsFactors = FALSE
)

# Combine all the data frames
all_starters <- bind_rows(all_starters, heat_starters, bucks_starters, timberwolves_starters, 
                          pelicans_starters, knicks_starters, thunder_starters, magic_starters, 
                          sixers_starters, suns_starters, trailblazers_starters, kings_starters, 
                          spurs_starters, raptors_starters, jazz_starters, wizards_starters)

# Replace abbreviated first names with full names in all_starters

all_starters$Player <- gsub("\\bJ. Clarkson\\b", "Jordan Clarkson", all_starters$Player)
all_starters$Player <- gsub("\\bD. Mitchell\\b", "Donovan Mitchell", all_starters$Player)
all_starters$Player <- gsub("\\bB. Bogdanovic\\b", "Bojan Bogdanovic", all_starters$Player)
all_starters$Player <- gsub("\\bL. Markkanen\\b", "Lauri Markkanen", all_starters$Player)
all_starters$Player <- gsub("\\bM. Conley\\b", "Mike Conley", all_starters$Player)
all_starters$Player <- gsub("\\bR. Jefferson\\b", "Richard Jefferson", all_starters$Player)
all_starters$Player <- gsub("\\bM. Williams\\b", "Marvin Williams", all_starters$Player)
all_starters$Player <- gsub("\\bG. Temple\\b", "Garrett Temple", all_starters$Player)
all_starters$Player <- gsub("\\bT. Satoransky\\b", "Tomas Satoransky", all_starters$Player)
all_starters$Player <- gsub("\\bA. Bradley\\b", "Avery Bradley", all_starters$Player)
all_starters$Player <- gsub("\\bP. Beverley\\b", "Patrick Beverley", all_starters$Player)
all_starters$Player <- gsub("\\bT. Young\\b", "Thaddeus Young", all_starters$Player)
all_starters$Player <- gsub("\\bC. Wood\\b", "Christian Wood", all_starters$Player)
all_starters$Player <- gsub("\\bP. Millsap\\b", "Paul Millsap", all_starters$Player)
all_starters$Player <- gsub("\\bR. Rubio\\b", "Ricky Rubio", all_starters$Player)
all_starters$Player <- gsub("\\bC. Boucher\\b", "Chris Boucher", all_starters$Player)
all_starters$Player <- gsub("\\bG. Trent Jr.\\b", "Gary Trent Jr.", all_starters$Player)
all_starters$Player <- gsub("\\bK. Porter Jr.\\b", "Kevin Porter Jr.", all_starters$Player)
all_starters$Player <- gsub("\\bP. Tucker\\b", "P.J. Tucker", all_starters$Player)

# Reformat 'Season' column in all_starters to match Basketball Reference format (e.g., "2022-23")
all_starters <- all_starters %>%
  mutate(Season = paste0(str_sub(Season, 1, 5), str_sub(Season, 8, 9)))

# Print all_starters to confirm all corrections
print(all_starters)


# Getting basketball refernece stats for each of the 150 players in each of the 10 seasons


# Create a lookup vector for team abbreviations to full names
team_lookup <- c(
  "ATL" = "Atlanta Hawks", "BOS" = "Boston Celtics", "BRK" = "Brooklyn Nets",
  "CHO" = "Charlotte Hornets","CHA" = "Charlotte Hornets", "CHI" = "Chicago Bulls", "CLE" = "Cleveland Cavaliers",
  "DAL" = "Dallas Mavericks", "DEN" = "Denver Nuggets", "DET" = "Detroit Pistons",
  "GSW" = "Golden State Warriors", "HOU" = "Houston Rockets", "IND" = "Indiana Pacers",
  "LAC" = "Los Angeles Clippers", "LAL" = "Los Angeles Lakers", "MEM" = "Memphis Grizzlies",
  "MIA" = "Miami Heat", "MIL" = "Milwaukee Bucks", "MIN" = "Minnesota Timberwolves",
  "NOP" = "New Orleans Pelicans", "NYK" = "New York Knicks", "OKC" = "Oklahoma City Thunder",
  "ORL" = "Orlando Magic", "PHI" = "Philadelphia 76ers", "PHO" = "Phoenix Suns",
  "POR" = "Portland Trail Blazers", "SAC" = "Sacramento Kings", "SAS" = "San Antonio Spurs",
  "TOR" = "Toronto Raptors", "UTA" = "Utah Jazz", "WAS" = "Washington Wizards"
)

# Modify the scrape_season_data function to add a formatted 'Season' column
scrape_season_data <- function(season) {
  season_formatted <- paste0(season - 1, "-", substr(season, 3, 4))
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, "_per_game.html")
  page <- read_html(url)
  table <- page %>%
    html_node(xpath = '//*[@id="per_game_stats"]') %>%
    html_table()
  
  # Clean and select columns, add formatted Season and Team columns
  stats <- table %>%
    filter(Player != "Player") %>% 
    mutate(Season = season_formatted) %>%
    dplyr::select(Player, Season, Pos, Age, MP, G, Team)  %>% 
    rename(MPG = MP) %>% 
    mutate(Age = as.numeric(Age),
           MPG = as.numeric(MPG))
  
  return(stats)
}

# Define the range of seasons to scrape
seasons <- 2014:2023

# Scrape data for each season with formatted 'Season' column
all_stats <- map_dfr(seasons, scrape_season_data)

# Apply the team name conversion to the 'Team' column in all_stats
all_stats <- all_stats %>%
  mutate(Team = recode(Team, !!!team_lookup))

# Load necessary library to handle accents
library(stringi)

# Create a version of all_stats where '2TM' is matched to any team
all_stats_clean <- all_stats %>%
  mutate(Team = ifelse(Team == "2TM", NA, Team)) %>%  # Replace "2TM" with NA for matching
  mutate(Player = stri_trans_general(Player, "Latin-ASCII"))  # Remove accents from Player names

# Remove accents from Player names in all_starters as well
all_starters_clean <- all_starters %>%
  mutate(Player = stri_trans_general(Player, "Latin-ASCII"))

# Perform the join, allowing 2TM players to match with any team and remove the Pos column
filtered_stats <- all_starters_clean %>%
  left_join(all_stats_clean, by = c("Player", "Season")) %>%
  filter(is.na(Team.y) | Team.x == Team.y) %>%  # Keep rows where Team.x matches or where Team.y is NA (for 2TM)
  dplyr::select(-Team.y, -Pos) %>%  # Remove extra Team column and Pos column
  rename(Team = Team.x)  # Rename Team.x back to Team for consistency

# Remove duplicates by keeping the row with the highest number of games played (G) for each Player and Season
filtered_stats <- filtered_stats %>%
  group_by(Player, Season) %>%
  arrange(desc(G)) %>%  # Order by descending games played
  slice(1) %>%          # Keep only the first row (highest games played)
  ungroup()

# View the updated filtered_stats without duplicates
View(filtered_stats)












# Part 2: get game by game minutes data

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



# Function to scrape game-by-game minutes for a player in a specific season
scrape_game_logs <- function(player_url, season) {
  # Remove ".html" from player_url if present
  player_url <- sub("\\.html$", "", player_url)
  
  # Convert the season format from "2015-16" to "2016" for the URL
  end_year <- as.integer(substr(season, 5, 6)) + 2000
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
    filter(str_starts(Date, "2")) %>%  # Keep rows where Date starts with "2"
    mutate(
      Season = season,  # Keep the original season format
      MP = sapply(str_split(MP, ":"), function(x) as.numeric(x[1]) + as.numeric(x[2]) * (1 / 60))  # Convert to decimal minutes
    ) %>%
    # Replace NA minutes with 0s for DNP games
    mutate(MP = ifelse(is.na(MP), 0, MP)) %>%
    # Calculate 5-game rolling average with adjustments for the first 4 games
    mutate(
      Rolling_Avg_MP = case_when(
        row_number() == 1 ~ MP,  # First game
        row_number() == 2 ~ mean(MP[1:2]),  # Second game
        row_number() == 3 ~ mean(MP[1:3]),  # Third game
        row_number() == 4 ~ mean(MP[1:4]),  # Fourth game
        TRUE ~ zoo::rollmean(MP, 5, fill = NA, align = "right")  # Regular 5-game rolling average from the fifth game onward
      )
    ) %>%
    dplyr::select(Date, MP, Rolling_Avg_MP, Season)  # Select only the necessary columns
  
  return(game_logs)
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


# Update the URL column in player_urls based on Player
player_urls <- player_urls %>%
  mutate(URL = case_when(
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
    TRUE ~ URL  # Keep existing URLs for other players
  ))



# Initialize an empty data frame to store all game logs
all_game_logs <- data.frame()

# Loop through each player-season combination and scrape game logs
for (i in seq_len(nrow(unique_players_seasons))) {
  player <- unique_players_seasons$Player[i]
  season <- gsub("-", "", unique_players_seasons$Season[i])  # Convert "2022-23" to "202223" format
  player_url <- player_urls$URL[player_urls$Player == player]
  
  if (!is.na(player_url)) {
    game_logs <- scrape_game_logs(player_url, season)
    print(game_logs)
    if (!is.null(game_logs)) {
      # Add player, season, and relevant team info from filtered_stats
      player_info <- filtered_stats %>%
        filter(Player == player, Season == unique_players_seasons$Season[i]) %>%
        distinct()  # Avoid duplicates
      
      game_logs <- game_logs %>%
        mutate(Player = player, Season = unique_players_seasons$Season[i]) %>%
        left_join(player_info, by = c("Player", "Season"))
      
      # Append to the combined data frame
      all_game_logs <- bind_rows(all_game_logs, game_logs)
    }
  }
  Sys.sleep(2) # Pause to avoid overloading the server
  
}

View(all_game_logs)

# Diagnosing missing players

# Extract the unique player-season combinations from filtered_stats and all_game_logs
player_season_in_filtered_stats <- unique(filtered_stats[, c("Player", "Season")])
player_season_in_all_game_logs <- unique(all_game_logs[, c("Player", "Season")])

# Find player-season combinations in filtered_stats but not in all_game_logs
missing_player_season <- dplyr::anti_join(player_season_in_filtered_stats, 
                                          player_season_in_all_game_logs, 
                                          by = c("Player", "Season"))

# Display the missing player-season combinations
print(missing_player_season)


# Adding game logs for missing players

# Define the list of manually retrieved game log URLs with player and season info
manual_game_logs <- data.frame(
  Player = c("Elfrid Payton", "Gerald Henderson", "Gerald Henderson", "Glenn Robinson", 
             "Ivica Zubac", "James Harden", "Jarell Martin", "Jerryd Bayless", 
             "Joe Ingles", "John Henson", "John Wall", "John Wall", 
             "John Wall", "John Wall", "John Wall", "John Wall", 
             "Robert Williams", "Robert Williams"),
  Season = c("2014-15", "2013-14", "2014-15", "2019-20", "2021-22", 
             "2022-23", "2017-18", "2017-18", "2016-17", "2017-18", 
             "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", 
             "2020-21", "2021-22", "2022-23"),
  URL = c("https://www.basketball-reference.com/players/p/paytoel01/gamelog/2015/",
          "https://www.basketball-reference.com/players/h/hendege02/gamelog/2014/",
          "https://www.basketball-reference.com/players/h/hendege02/gamelog/2015/",
          "https://www.basketball-reference.com/players/r/robingl02/gamelog/2020/",
          "https://www.basketball-reference.com/players/z/zubaciv01/gamelog/2022/",
          "https://www.basketball-reference.com/players/h/hardeja01/gamelog/2023/",
          "https://www.basketball-reference.com/players/m/martija01/gamelog/2018/",
          "https://www.basketball-reference.com/players/b/bayleje01/gamelog/2018/",
          "https://www.basketball-reference.com/players/i/inglejo01/gamelog/2017/",
          "https://www.basketball-reference.com/players/h/hensojo01/gamelog/2018/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2014/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2015/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2016/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2017/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2018/",
          "https://www.basketball-reference.com/players/w/walljo01/gamelog/2021/",
          "https://www.basketball-reference.com/players/w/williro04/gamelog/2022/",
          "https://www.basketball-reference.com/players/w/williro04/gamelog/2023/")
)

# Loop through each manual game log entry
for (i in seq_len(nrow(manual_game_logs))) {
  player <- manual_game_logs$Player[i]
  season <- manual_game_logs$Season[i]
  url <- manual_game_logs$URL[i]
  
  # Try reading the page and handle errors if any
  page <- tryCatch(rvest::read_html(url), error = function(e) NULL)
  if (is.null(page)) next  # Skip if the page couldn't be loaded
  
  # Attempt to extract the table with id "pgl_basic", handle missing table by skipping
  game_logs <- tryCatch(
    page %>%
      html_node("#pgl_basic") %>%
      html_table(fill = TRUE),
    error = function(e) NULL
  )
  
  # If the table is missing, skip to the next iteration
  if (is.null(game_logs)) next
  
  # Ensure all columns have names
  colnames(game_logs) <- make.names(colnames(game_logs), unique = TRUE)
  
  # Filter out rows where 'Date' does not start with "2" and process minutes
  game_logs <- game_logs %>%
    filter(str_starts(Date, "2")) %>%  # Keep rows where Date starts with "2"
    mutate(
      Season = season,  # Keep the original season format
      MP = sapply(str_split(MP, ":"), function(x) as.numeric(x[1]) + as.numeric(x[2]) * (1 / 60))  # Convert to decimal minutes
    ) %>%
    # Replace NA minutes with 0s for DNP games
    mutate(MP = ifelse(is.na(MP), 0, MP)) %>%
    # Calculate 5-game rolling average with adjustments for the first 4 games
    mutate(
      Rolling_Avg_MP = case_when(
        row_number() == 1 ~ MP,  # First game
        row_number() == 2 ~ mean(MP[1:2]),  # Second game
        row_number() == 3 ~ mean(MP[1:3]),  # Third game
        row_number() == 4 ~ mean(MP[1:4]),  # Fourth game
        TRUE ~ zoo::rollmean(MP, 5, fill = NA, align = "right")  # Regular 5-game rolling average from the fifth game onward
      )
    ) %>%
    mutate(Player = player) %>%
    dplyr::select(Date, MP, Rolling_Avg_MP, Season, Player)  # Select only the necessary columns
  
  # Append to the combined data frame
  all_game_logs <- bind_rows(all_game_logs, game_logs)
  Sys.sleep(2) # Pause to avoid overloading the server
}


# Fixing unknown issue with missing age, mpg and g data from filtered_stats

# Filter for player-season combinations with any NA values in filtered_stats
missing_data_combos <- filtered_stats %>%
  filter(if_any(everything(), is.na)) %>%
  distinct(Player, Season)

View(missing_data_combos)




# Function to retrieve stats for a specific season from a player
# Function to retrieve stats for a specific season from a player’s URL
get_player_season_stats <- function(player_url, season_year) {
  # Load the player page
  print(player_url)
  page <- tryCatch(read_html(player_url), error = function(e) NA)
  
  # Check if page loaded successfully
  if (is.na(page)) {
    return(data.frame(Age = NA, G = NA, MPG = NA))
  }
  
  # Locate the "Per Game" stats table and filter by season
  per_game_table <- page %>%
    html_node("#per_game_stats") %>%
    html_table(fill = TRUE)
  
  # Convert table to data frame and filter for the desired season
  stats <- per_game_table %>%
    filter(Season == season_year) %>%
    dplyr::select(Age = Age, G = G, MPG = MP)
  
  # Check if data for the season was found, else return NA row
  if (nrow(stats) == 0) {
    stats <- data.frame(Age = NA, G = NA, MPG = NA)
  }
  
  # Return stats
  return(stats)
}

# Loop through each player-season combo in missing_data_combos and update filtered_stats
for (i in 1:nrow(missing_data_combos)) {
  player <- missing_data_combos$Player[i]
  season <- missing_data_combos$Season[i]
  player_url <- player_urls$URL[player_urls$Player == player]
  
  # Only proceed if player_url is available
  if (length(player_url) > 0) {
    # Retrieve stats with delay
    Sys.sleep(2)  # Adjust delay as needed to avoid rate limiting
    season_stats <- get_player_season_stats(player_url, season)
    print(season_stats)
    # Find rows in filtered_stats for the specific player and season
    rows_to_update <- which(filtered_stats$Player == player & 
                              filtered_stats$Season == season)
    
    # Update Age, G, and MPG only if they are NA in filtered_stats
    if (length(rows_to_update) > 0) {
      if (is.na(filtered_stats$Age[rows_to_update])) {
        filtered_stats$Age[rows_to_update] <- as.numeric(season_stats$Age)
      }
      if (is.na(filtered_stats$G[rows_to_update])) {
        filtered_stats$G[rows_to_update] <- as.numeric(season_stats$G)
      }
      if (is.na(filtered_stats$MPG[rows_to_update])) {
        filtered_stats$MPG[rows_to_update] <- as.numeric(season_stats$MPG)
      }
    }
  }
}


#Fix Glenn Robinson III in 2019-20 (url went to his dad)
filtered_stats <- filtered_stats %>%
  mutate(
    Age = if_else(Player == "Glenn Robinson" & Season == "2019-20", 26, Age),
    G = if_else(Player == "Glenn Robinson" & Season == "2019-20", 62, G),
    MPG = if_else(Player == "Glenn Robinson" & Season == "2019-20", 28.8, MPG)
  )

# Display the updated filtered_stats data
View(filtered_stats)

# Check for any NA values in the filtered_stats data frame
na_summary <- colSums(is.na(filtered_stats))
na_summary


# Add all info from filtered stats to all_game_logs

# Select the necessary columns from filtered_stats to update the missing information
player_info_update <- filtered_stats %>%
  dplyr::select(Player, Season, Team, Position, Age, MPG, G) %>%
  distinct()  # Ensure there are no duplicate rows

# Update all_game_logs by performing a left_join with player_info_update
all_game_logs <- all_game_logs %>%
  left_join(player_info_update, by = c("Player", "Season"))

# Remove columns with .y suffix and rename .x columns to their original names
all_game_logs <- all_game_logs %>%
  dplyr::select(-ends_with(".x")) %>% # Remove all columns ending in .y
  dplyr::rename_with(~ gsub("\\.y$", "", .), ends_with(".y")) # Rename .x columns by removing .x suffix

# Display the updated all_game_logs data frame
View(all_game_logs)

# Check for any NA values in the filtered_stats data frame
na_summary <- colSums(is.na(all_game_logs))
na_summary








# Part 3: Using Airball package to get travel and injury data

library(airball)

# Increase the connection buffer size
Sys.setenv("VROOM_CONNECTION_SIZE" = "5000000")  # Set to 5,000,000 bytes, or adjust as needed

# Function to retrieve travel data for each player-season combination
get_travel_metrics <- function(all_game_logs) {
  # Extract unique player-season combos
  player_season_combos <- all_game_logs %>%
    distinct(Player, Season)
  
  # Initialize an empty list to store travel data
  travel_data_list <- list()
  
  # Loop through each player-season combo
  for (i in seq_len(nrow(player_season_combos))) {
    player <- player_season_combos$Player[i]
    season <- player_season_combos$Season[i]
    print(player)
    print(season)
    # Calculate the correct season end year (e.g., "2017-18" should map to 2018)
    season_end_year <- as.numeric(substr(season, 6, 7)) + 2000
    
    # Extract travel metrics using nba_player_travel
    travel_data <- nba_player_travel(
      start_season = season_end_year,
      end_season = season_end_year,
      player = player
    )
    print(travel_data)
    travel_data_list[[i]] <- travel_data
  }
  
  # Combine all travel data into a single dataframe
  travel_data_combined <- bind_rows(travel_data_list)
  
  return(travel_data_combined)
}

# Retrieve and inspect travel data
travel_data <- get_travel_metrics(all_game_logs)
View(travel_data)  # Inspect travel data to ensure accuracy


# Ensure both datasets have consistent column names for player-season combinations

# Find player-season combinations in filtered_stats but not in travel_data
missing_combos <- filtered_stats %>%
  dplyr::select(Player, Season) %>% # Select relevant columns
  distinct() %>%            # Ensure unique player-season combinations
  anti_join(travel_data %>% select(Player, Season) %>% distinct(), by = c("Player", "Season")) # Find missing

# Save or inspect
View(missing_combos) # View in RStudio (optional)

library(tidyr)

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
  "Kelly Oubre, Jr." = "Kelly Oubre Jr.",
  "Kenyon Martin Jr." = "KJ Martin",
  "Kevin Knox" = "Kevin Knox II",
  "Luka Doncic" = "Luka Dončić",
  "Kristaps Porzingis" = "Kristaps Porziņģis",
  "Lonnie Walker" = "Lonnie Walker IV",
  "Marcus Morris" = "Marcus Morris Sr.",
  "Moe Harkless" = "Maurice Harkless",
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
  "Wendell Carter, Jr." = "Wendell Carter Jr.",
  "Xavier Tillman, Sr." = "Xavier Tillman"
)

# Update player names in the missing_data_combos using the mapping
missing_combos <- missing_combos %>%
  mutate(Player = ifelse(Player %in% names(name_mapping), name_mapping[Player], Player))

# Function to retrieve travel metrics for missing player-season combinations
get_missing_travel_metrics <- function(missing_data_combos) {
  # Initialize an empty list to store travel data
  travel_data_list <- list()
  
  # Loop through each player-season combo
  for (i in seq_len(nrow(missing_data_combos))) {
    player <- missing_data_combos$Player[i]
    season <- missing_data_combos$Season[i]
    print(paste("Processing:", player, "-", season))
    
    # Calculate the correct season end year (e.g., "2017-18" should map to 2018)
    season_end_year <- as.numeric(substr(season, 6, 7)) + 2000
    
    # Attempt to retrieve travel metrics using nba_player_travel
    tryCatch({
      travel_data <- nba_player_travel(
        start_season = season_end_year,
        end_season = season_end_year,
        player = player
      )
      
      # Append retrieved data to the list
      travel_data_list[[i]] <- travel_data
    }, error = function(e) {
      # Print a warning for failed attempts
      warning(paste("Failed to retrieve data for:", player, "-", season, ":", e$message))
    })
  }
  
  # Combine all travel data into a single dataframe
  travel_data_combined <- bind_rows(travel_data_list)
  
  return(travel_data_combined)
}

# Usage example
new_travel_data <- get_missing_travel_metrics(missing_combos)

# Combine new travel data with existing travel data
travel_data <- bind_rows(travel_data, new_travel_data)

# Display the updated travel data, should now contain all rows
View(travel_data)


#Retrieve injury transactions

# Initialize an empty list to store injury data
injury_data_list <- list()

# Update player names in the missing_data_combos using the mapping
filtered_stats_accents <- filtered_stats %>%
  mutate(Player = ifelse(Player %in% names(name_mapping), name_mapping[[Player]], Player))

# Get unique player-season combinations
player_season_combos <- filtered_stats_accents %>%
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

# Export filtered_stats
write.csv(filtered_stats, "filtered_stats.csv", row.names = FALSE)

# Export filtered_stats_accents
write.csv(filtered_stats_accents, "filtered_stats_accents.csv", row.names = FALSE)

# Export all_game_logs
write.csv(all_game_logs, "all_game_logs.csv", row.names = FALSE)

# Export travel_data
write.csv(travel_data, "travel_data.csv", row.names = FALSE)

# Export injury_data
write.csv(injury_data, "injury_data.csv", row.names = FALSE)

# Print confirmation
cat("Export completed. Files saved in the current working directory.\n")
