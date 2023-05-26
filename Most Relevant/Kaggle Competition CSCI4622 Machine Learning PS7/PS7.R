# Load required packages
library(xgboost)
library(magrittr)
library(dplyr)
library(tidyr)
library(caret)

#Import data
X <- read.csv("XTrain.csv")
y <- read.csv("YTrain.csv")

# Step 1: Find all unique genres
genres = list("none")
allGenres = list()
for (genre in X$genre){
  genre_list <- strsplit(gsub("[[:punct:]]", "", genre), ",")
  if(length(unlist(genre_list)) > 0){
    genre_list2 <- strsplit(unlist(genre_list), " ")[[1]]
    allGenres = append(allGenres,list(genre_list2))
    for (x in genre_list2){
      genres = append(genres,x)
    }
  }else{
    allGenres = append(allGenres,list("none"))
  }
}
unique_genres <- unique(genres)
unique_genres <- unlist(unique_genres, recursive = FALSE)


# Step 2: Create new binary columns for each unique genre
for (genre in unique_genres) {
  X[[genre]] <- 0
}

# Step 3: Set binary column values based on genre list for each movie
allGenres[6362] = list('Thriller','Drama')
for (i in 1:nrow(X)) {
  movie_genres <- unique(allGenres[i])
  for(genre in movie_genres){
    X[i,genre] <- 1
  }
}

#make language one hot encode vars
X$en <- ifelse(X$original_language == "en", 1, 0)
X$ja <- ifelse(X$original_language == "ja", 1, 0)
X$es <- ifelse(X$original_language == "es", 1, 0)
X$fr <- ifelse(X$original_language == "fr", 1, 0)
X$ko <- ifelse(X$original_language == "ko", 1, 0)
X$zh <- ifelse(X$original_language == "zh", 1, 0)
X$it <- ifelse(X$original_language == "it", 1, 0)
X$cn <- ifelse(X$original_language == "cn", 1, 0)
X$de <- ifelse(X$original_language == "de", 1, 0)
X$ru <- ifelse(X$original_language == "ru", 1, 0)
X$pt <- ifelse(X$original_language == "pt", 1, 0)
X$hi <- ifelse(X$original_language == "hi", 1, 0)
X$da <- ifelse(X$original_language == "da", 1, 0)
X$no <- ifelse(X$original_language == "no", 1, 0)
X$sv <- ifelse(X$original_language == "sv", 1, 0)

X$release_year <- as.integer(format(as.Date(X$release_date, "%m/%d/%Y"), "%Y"))
X$release_month <- as.integer(format(as.Date(X$release_date, "%m/%d/%Y"), "%m"))

#Filter out rows with 0 runtime and revenue
#X <- X[X$revenue > 0, ]
#X <- X[X$runtime > 0, ]

X <- merge(X, y, by.x = "id", by.y = "X")

X <- X %>%
  select_if(is.numeric)

y <- X$vote_average
X <- subset(X, select = -c(vote_average))

#Split into training and test data
set.seed(123)
trainIndex <- sample(nrow(X), 0.8 * nrow(X))
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)

# Train the model using these parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 7,
  eta = 0.1,
  min_child_weight = 10,
  subsample=0.7
)
dtrain <- xgb.DMatrix(X_train, label = y_train)
model <- xgboost(data = dtrain, params = params, nrounds = 75)

# Predict
pred <- predict(model, newdata = X_test)

# Calculate R-squared
SSres <- sum((y_test - pred)^2)
SStot <- sum((y_test - mean(y_test))^2)
r_squared <- 1 - SSres/SStot
r_squared

X_test <- read.csv("XTest.csv")

allGenresTest = list()
for (genre in X_test$genre){
  genre_list <- strsplit(gsub("[[:punct:]]", "", genre), ",")
  if(length(unlist(genre_list)) > 0){
    genre_list2 <- strsplit(unlist(genre_list), " ")[[1]]
    allGenresTest = append(allGenresTest,list(genre_list2))
  }else{
    allGenresTest = append(allGenresTest,list("none"))
  }
}

for (genre in unique_genres) {
  X_test[[genre]] <- 0
}

for (i in 1:nrow(X_test)) {
  movie_genres <- unique(allGenresTest[i])
  for(genre in movie_genres){
    X_test[i,genre] <- 1
  }
}

#make language one hot encode vars
X_test$en <- ifelse(X_test$original_language == "en", 1, 0)
X_test$ja <- ifelse(X_test$original_language == "ja", 1, 0)
X_test$es <- ifelse(X_test$original_language == "es", 1, 0)
X_test$fr <- ifelse(X_test$original_language == "fr", 1, 0)
X_test$ko <- ifelse(X_test$original_language == "ko", 1, 0)
X_test$zh <- ifelse(X_test$original_language == "zh", 1, 0)
X_test$it <- ifelse(X_test$original_language == "it", 1, 0)
X_test$cn <- ifelse(X_test$original_language == "cn", 1, 0)
X_test$de <- ifelse(X_test$original_language == "de", 1, 0)
X_test$ru <- ifelse(X_test$original_language == "ru", 1, 0)
X_test$pt <- ifelse(X_test$original_language == "pt", 1, 0)
X_test$hi <- ifelse(X_test$original_language == "hi", 1, 0)
X_test$da <- ifelse(X_test$original_language == "da", 1, 0)
X_test$no <- ifelse(X_test$original_language == "no", 1, 0)
X_test$sv <- ifelse(X_test$original_language == "sv", 1, 0)

X_test$release_year <- as.integer(format(as.Date(X_test$release_date, "%m/%d/%Y"), "%Y"))
X_test$release_month <- as.integer(format(as.Date(X_test$release_date, "%m/%d/%Y"), "%m"))

X_test <- X_test %>%
  select_if(is.numeric)
X_test <- as.matrix(X_test)
pred <- predict(model, newdata = X_test)

submission <- read.csv("YTest_Zeros.csv")
submission$vote_average = pred
View(submission)
write.csv(submission,file="submission.csv",row.names=FALSE)
