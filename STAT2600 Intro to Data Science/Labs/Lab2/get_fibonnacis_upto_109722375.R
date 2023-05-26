get_fibonnacis_upto <- function(upto){
  index = 1
  oneBehind = 0
  twoBehind = 0
  fibonnacis = vector()
  while (index <= upto){
    fibonnacis <- append(fibonnacis, index)
    twoBehind = oneBehind
    oneBehind = index
    index = oneBehind + twoBehind
  }
  return(fibonnacis)
}
get_fibonnacis_upto(100)