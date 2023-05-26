#### Homework 4/Lab 12 Q's ####

## generating plots in R script to transfer over to markdown
library(readr)
library(dplyr)

summaryDf = read_csv("Labs/Lab12_20221108/summary_df_lab12.csv")
summaryDf %>% View()


resultsDf = read_csv("Labs/Lab12_20221108/results_dataframe_lab12.csv")
resultsDf %>% 
    group_by(
        PropTrain
    ) %>% 
    summarise(
        RMSE_mean = mean(RMSE),
        RMSE_lb = quantile(RMSE, prob = .1),
        RMSE_ub = quantile(RMSE, prob = .9)
    )


quantile(resultsDf$RMSE, probs = c(.1, .9))

par(mfrow = c(1, 2))
with(summaryDf, 
     plot(PropTrain, RMSE_mean, type = "l",
          col = "blue",
          main = "Mean RMSE by Train Proportion",
          xlab = "Training Set Proportion", ylab = "RMSE")
)

with(summaryDf, 
     plot(PropTrain, RMSE_mean, type = "l",
          ylim = c(min(RMSE_lb), max(RMSE_ub)),
          col = "blue",
          main = "Mean RMSE w/ 80% Interval",
          xlab = "Training Set Proportion", ylab = ""
    )
)

with(summaryDf, lines(PropTrain, RMSE_lb, lty = 2, col = "blue"))
with(summaryDf, lines(PropTrain, RMSE_ub, lty = 2, col = "blue"))

par(mfrow = c(1, 1))
plot(mtcars$wt, mtcars$mpg)
