## This function creates a matrix-object through 2 functions and can cache
## it's inverse through a pair of get-set functions

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  ## Function to set the data, default the inverse to NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get the data
  get <- function() x
  
  ## Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse

  ## Function to get the inverse
  getInverse <- function() inv
  
  ## Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
    
}


## The cacheSolve function below returns the inverse of a matrix. If the
## inverse has not been calculated it will calculate the inverse and store it
## otherwise it will return the stored value

cacheSolve <- function(x, ...) {
  
  ## First try to get the cached inverse
  inv <- x$getInverse()
  
  ## If the cached value is not null, give a message. Otherwise, store the data
  ## then calculate the inverse and store it
  if(!is.null(inv)){
    message("returning cached inverse")
    return(inv)
  } else {
    data = x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
  }
  
  ## Return the inverse
  inv
}
