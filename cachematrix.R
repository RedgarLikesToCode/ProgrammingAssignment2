## makeacheMatrix is a special type of matrix that stores its inverse.
## cacheSolve is a special function that retrives the data in makeCacheMatrix and returns it/ calculates the inverse and then prints it.

## This function is designed to store the inverse of the given matrix "x"
x <- matrix((round(rnorm(9), 2)), byrow = T, nrow = 3)

makeCacheMatrix <- function(x) {

          
  set  <- function(y){
  
  x <<- y
    
  inv <<- NULL  
  }
    
  get <- function() x
  
   setinverse <- function(inverse) inv <<- inverse 
    
   getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
        
}


## cacheSolve will compute and return/ return the inverse of the given matrix

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("Getting Cached Data!")
    
    
    return(inv)
  }
  
  data <- x$get()
  
  inv <- inverse(data, ...)

  x$setinverse(inv)
  
  inv
}
