set.seed(100)
nn <- 1000
zz <- 4 + arima.sim(model=list(ar = .7, ma = -.5), n=nn)
##zz <- 4 + ts(sarima::sim_sarima(model=list(ar=.7, ma=-.5), n=nn, xintercept=0))
xx <- zz^4 ## 4 is very extreme; data plot is extreme
