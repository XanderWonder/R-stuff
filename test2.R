primenum <- function(num1){
  count <- numeric(length = length(num1))
  for(i in seq_along(num1)){
      if(i%%2 == 0 | i%%3 == 0 | i%%5 == 0 | i%%7 == 0){
        print(num1)
      }
      print(count) 
    }
}
primenum(1:10)
