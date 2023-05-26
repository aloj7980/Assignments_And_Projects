#### Lab 13 ####

#remove.packages("rlang")
#install.packages("rlang")
#library(rlang)
#library(dplyr)

library(dplyr)
library(readr)
library(tibble)

# move_nas_to_bottom <- function(col){
#     naInds = which(is.na(col))
#     nonNaInds = which(!is.na(col))
#     return(col[c(nonNaInds, naInds)])
# }
# 
# 
# nSamps = 6.6*10^4
# 
# sampleMatrix_all = cbind(
#     Sample1 = runif(nSamps, min = -1, max = 1.01),
#     Sample2 = rnorm(nSamps, mean = .04, sd = 5),
#     Sample3 = rexp(nSamps, rate = 2) - 99/200,
#     Sample4 = (rgamma(nSamps, shape = 2, rate = 3) - .66),
#     Sample5 = rnorm(nSamps, mean = .004)
# )
# nNas = 5*10^4
# naInds = cbind(
#     sample(nrow(sampleMatrix_all), size = nNas, replace = TRUE),
#     sample(ncol(sampleMatrix_all), size = nNas, replace = TRUE, 
#            prob = sample(ncol(sampleMatrix_all)))
# )
# sampleMatrix_all[naInds] = NA
# sampleMatrix = sampleMatrix_all %>%
#     apply(2, move_nas_to_bottom)
# 
# sampleMatrix = sampleMatrix[apply(sampleMatrix, 1, function(r){any(!is.na(r))}), ]
# 
# sampleMatrix %>%
#     as.data.frame() %>%
#     write_csv("Labs/Lab13_20221115/sample_data_lab13.csv")




# Import data

#my.directory <- "C:/Users/zachs/Downloads"
#setwd(my.directory)
sampleMatrix = read_csv("sample_data_lab13.csv")

dim(sampleMatrix)
head(sampleMatrix)

# Visualize the data with histograms

par(mar = c(1, 1, 1, 1))
hist(sampleMatrix$Sample1, col = '#E62525', breaks = 60)
hist(sampleMatrix$Sample2, col = '#FFA820', breaks = 60)
hist(sampleMatrix$Sample3, col = '#1DCA38', breaks = 60)
hist(sampleMatrix$Sample4, col = '#00AD9E', breaks = 60)
hist(sampleMatrix$Sample5, col = '#3600AD', breaks = 60)

# analyze_sample function
# inputs a vector 
# outputs a vector of length 6

analyze_sample <- function(s){
  s = s[!is.na(s)]
  c(
    # sample size
    N = length(s),
    # sample mean
    Mean = mean(s),
    # sample standard deviation
    SD = sd(s),
    # sample standard error
    SE = sd(s)/sqrt(length(s)),
    # hypothesis testing: 
    # sample mean is (approximately) normally distributed by Central Limit Theorem
    # H_0: mu = 0
    # H_1: mu > 0 
    # test statistic for hypothesis test
    TestStat = mean(s)/(sd(s)/sqrt(length(s))),
    # p-value for hypothesis test
    PValue = pnorm(mean(s)/(sd(s)/sqrt(length(s))), lower.tail = FALSE)
  )
}

# construct summaryMatrix

# apply the analyze_sample function to the sampleMatrix, by column
summaryMatrix = apply(sampleMatrix, 2, analyze_sample) %>% 
  # transpose the matrix
  t() %>% 
  # coerce data frame to tibble
  as_tibble() %>% 
  # add sample column
  add_column(
    Sample = colnames(sampleMatrix), 
    .before = "N"
  ) 

summaryMatrix %>% View()

summaryMatrix %>% write_csv("lab13.csv")
