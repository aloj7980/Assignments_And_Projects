set.seed(100)
nn <- 1000
zz <- 4+ ts(sim_sarima(model=list(ar=.7, ma=-.5), n=nn, xintercept=.1))
xx <- zz^4
