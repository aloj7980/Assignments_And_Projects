
#### NOTE: it matters whether variables are matrices or not !  will break if not

locallevel_2_1 <- function(x, m0){
  Sigma.R <- diag(c(exp(x[1]), exp(x[2]))) ## obs
  ## Sigma.Q <- diag(c(exp(x[3]), exp(x[4]))) ## state
  Sigma.Q <- matrix(exp(x[3]))
  m0 <- as.matrix(m0)
  ## C0 <- diag(c(0,0))
  C0 <- matrix(data=.1^2)
  ##  GG <- diag(c(1, 1)) ## state
  GG <- matrix(data=1)
  FF <- matrix(c(1,1), ncol=1)
  return(list(
    m0=m0,
    C0=C0,
    FF=FF,
    GG=GG,
    V=Sigma.R,
    W=Sigma.Q))}

locallevel_2_1_v2 <- function(x, m0){
  Sigma.R <- diag(c(exp(x[1]), exp(x[1]))) ## obs
  ## Sigma.Q <- diag(c(exp(x[3]), exp(x[4]))) ## state
  Sigma.Q <- matrix(exp(x[2]))
  m0 <- m0
  ## C0 <- diag(c(0,0))
  C0 <- matrix(.1^2)
  ##  GG <- diag(c(1, 1)) ## state
  GG <- matrix(1) ## diag(1);
  FF <- matrix(c(1,1), ncol=1)
  return(list(
    m0=m0,
    C0=C0,
    FF=FF,
    GG=GG,
    V=Sigma.R,
    W=Sigma.Q))}


locallevel_1_1 <- function(x, m0){
  Sigma.R <- matrix(exp(x[1])) ## obs
  ## Sigma.Q <- diag(c(exp(x[3]), exp(x[4]))) ## state
  Sigma.Q <- matrix(exp(x[2]))
  m0 <- m0
  ## C0 <- diag(c(0,0))
  C0 <- matrix(.2^2)
  ##  GG <- diag(c(1, 1)) ## state
  GG <- matrix(1) ## diag(1);
  FF <- matrix(1) ## diag(c(1))
  return(list(
    m0=m0,
    C0=C0,
    FF=FF,
    GG=GG,
    V=Sigma.R,
    W=Sigma.Q))}






