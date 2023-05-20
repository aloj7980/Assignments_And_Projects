library(dplyr)

# Load data into a data frame
pitcher_data <- read.csv("stats.csv")

#Filter for min batters faced
pitcher_data <- subset(pitcher_data, p_total_pa >= 200)

# Calculate ground ball percentage
pitcher_data$GB_pct <- (pitcher_data$p_out_ground + pitcher_data$p_hit_ground) / pitcher_data$p_total_pa

# Calculate line drive percentage
pitcher_data$LD_pct <- (pitcher_data$p_out_line_drive + pitcher_data$p_hit_line_drive)  / pitcher_data$p_total_pa

# Calculate fly ball percentage
pitcher_data$FB_pct <- (pitcher_data$p_out_fly + pitcher_data$p_out_line_drive) / pitcher_data$p_total_pa

#Filter to columns to be used for estimation and ERA
pitcher_data <- subset(pitcher_data, select = c(FB_pct, GB_pct, LD_pct, p_k_percent, p_bb_percent, p_era))

# Calculate squares of variables
pitcher_data$GB_pct2 <- pitcher_data$GB_pct^2
pitcher_data$LD_pct2 <- pitcher_data$LD_pct^2
pitcher_data$FB_pct2 <- pitcher_data$FB_pct^2
pitcher_data$p_k_percent2 <- pitcher_data$p_k_percent^2
pitcher_data$p_bb_percent2 <- pitcher_data$p_bb_percent^2

# Calculate all possible products of variables
pitcher_data$GB_pct_LD_pct <- pitcher_data$GB_pct * pitcher_data$LD_pct
pitcher_data$GB_pct_FB_pct <- pitcher_data$GB_pct * pitcher_data$FB_pct
pitcher_data$GB_pct_p_k_percent <- pitcher_data$GB_pct * pitcher_data$p_k_percent
pitcher_data$GB_pct_p_bb_percent <- pitcher_data$GB_pct * pitcher_data$p_bb_percent
pitcher_data$LD_pct_FB_pct <- pitcher_data$LD_pct * pitcher_data$FB_pct
pitcher_data$LD_pct_p_k_percent <- pitcher_data$LD_pct * pitcher_data$p_k_percent
pitcher_data$LD_pct_p_bb_percent <- pitcher_data$LD_pct * pitcher_data$p_bb_percent
pitcher_data$FB_pct_p_k_percent <- pitcher_data$FB_pct * pitcher_data$p_k_percent
pitcher_data$FB_pct_p_bb_percent <- pitcher_data$FB_pct * pitcher_data$p_bb_percent
pitcher_data$p_k_percent_p_bb_percent <- pitcher_data$p_k_percent * pitcher_data$p_bb_percent

# Split data into training and testing sets
set.seed(18)
train_index <- sample(nrow(pitcher_data), size = round(0.7 * nrow(pitcher_data)), replace = FALSE)
train_data <- pitcher_data[train_index, ]
test_data <- pitcher_data[-train_index, ]

# Fit forward stepwise model using p_era as response variable
forward_model <- lm(p_era ~ 1, data = train_data)
forward_model <- step(forward_model, direction = "forward", scope = list(lower = ~ 1, upper = ~ .), trace = 0)

# Fit backward stepwise model using p_era as response variable
backward_model <- lm(p_era ~ FB_pct + GB_pct + LD_pct + p_k_percent + p_bb_percent, data = train_data)
backward_model <- step(backward_model, direction = "backward", trace = 0)

# Print summary of models
summary(forward_model)
summary(backward_model)

# Predict p_era for test data using both models
forward_preds <- predict(forward_model, newdata = test_data)
backward_preds <- predict(backward_model, newdata = test_data)

# Calculate RMSE for both models
forward_rmse <- sqrt(mean((test_data$p_era - forward_preds)^2))
backward_rmse <- sqrt(mean((test_data$p_era - backward_preds)^2))

# Print RMSE for both models
print(paste0("Forward model RMSE: ", forward_rmse))
print(paste0("Backward model RMSE: ", backward_rmse))

# Predict response variable on the test set
test_pred <- predict(backward_model, newdata = test_data)

# Calculate the residual sum of squares (RSS) and total sum of squares (TSS)
RSS <- sum((test_data$p_era - test_pred)^2)
TSS <- sum((test_data$p_era - mean(train_data$p_era))^2)

# Calculate the number of observations and number of predictor variables
n <- nrow(test_data)
p <- length(backward_model$coefficients) - 1

# Calculate the adjusted R-squared on the test set
adj_r_squared <- 1 - (RSS / (n - p - 1)) / (TSS / (n - 1))

# Print the adjusted R-squared value
cat("Adjusted R-squared on test set:", round(adj_r_squared, 3), "\n")


