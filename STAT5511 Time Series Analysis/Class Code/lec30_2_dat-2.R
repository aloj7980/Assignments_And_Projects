## Heavy tailed.  The usual procedures in this case do actually seem to work.
## Data transformations seem to help in diagnostics but 'wrong' model
## selected.  (And what does the model mean anyway?)
####
## If transformation done, the centering matters.
set.seed(102)
nn <- 500
zz <- 7 + arima.sim(model=list(ar = c(.6, .3), ma = c(-.5, -.4)),
                    rand.gen = rt,
                    n=nn,
                    df=3)
xx <- zz
