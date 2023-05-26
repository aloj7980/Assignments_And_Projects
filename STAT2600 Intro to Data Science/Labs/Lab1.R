sum100 <- sum(1:100)
sumSquares100 <- sum((1:10000)[sqrt(1:10000) == as.integer(sqrt(1:10000))])
sum100Squared <- sum100^2
print(sum100Squared - sumSquares100)
