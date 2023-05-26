## a function that returns TRUE if n is a palindrome and FALSE if it is not
is_palindrome <- function(n){
  digsOfN = unlist(strsplit(as.character(n), ""))
  return(
    all(digsOfN == rev(digsOfN))
  )
}
## a function that returns TRUE if n is a product of two three digit numbers
## and FALSE if it is not
is_product_of_3DigNums <- function(z){
   for(x in 100:999){
     for(y in 100:999){
       if((x*y) == z){
         return(TRUE)
       }
     }
   }
  return(FALSE)
}
index <- 999 * 999 ##Starts at largest product of three digit numbers and iterates down
found <- FALSE
while(!found){
  if(is_palindrome(index) && is_product_of_3DigNums(index)){
    print(index)
    found = TRUE
  }
  index = index - 1
}

