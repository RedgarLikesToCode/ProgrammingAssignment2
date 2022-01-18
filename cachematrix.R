# makeCacheMatrix: This function creates a special "matrix" object that can cache 
#its inverse.

# cacheSolve: This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed),then the cachesolve should retrieve
# the inverse from the cache

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


CacheSolve <- function(x, ...) {
  
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
