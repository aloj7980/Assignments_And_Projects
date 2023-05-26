## function that takes in an integer >= 2 and returns TRUE if it is prime or FALSE if it is comp
is_prime <- function(n){ 
  if((round(n) != n) | (n <= 0)){
    stop("`n` must be a positive integer.")} 
  if(n == 1){ ## 1 is neither prime nor composite
    return(NA)} 
  if(n %in% c(2, 3)){
      return(TRUE)}
  ## all numbers less than sqrt(n) are not factors of n
  return(
    all(n %% 2:floor(sqrt(n)) != 0) )
}
## function that takes in a positive integer and returns a vector of its factors
factor_integer <- function(n){ 
  if((round(n) != n) | (n <= 0)){
    stop("`n` must be a positive integer.")}
  ## which numbers less than sqrt(n) are factors
  factLeqSqrt = which(n %% 1:floor(sqrt(n)) == 0)
  factGeqSqrt = n / factLeqSqrt
  ## concatenates; removes sqrt(n) duplicate (if applicable)
  factors = unique(c(factLeqSqrt, rev(factGeqSqrt)))
  return(factors)
}
largest_prime_factor <- function(n){
  factors = factor_integer(n)
  currentLargestPrime = 0
  for(factor in factors){
    if(factor > 1 & is_prime(factor) & (factor > currentLargestPrime)){
      currentLargestPrime = factor
    }
  }
  return(currentLargestPrime)
}
pe3_answer = largest_prime_factor(600851475143)

ten_thousand_first_prime <- function(){
  count = 0
  index = 2
  while(count < 10001){
    if(is_prime(index)){
      count = count + 1
    }
    index = index + 1
  }
  return(index - 1)
}
pe7_answer = ten_thousand_first_prime()
