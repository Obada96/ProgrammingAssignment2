## Put comments here that give an overall description of what your
## functions do
## OW Week 3 Assignment Jun 2020
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## define the argument with default mode of "matrix"
  inv <- NULL                              
  set <- function(y) {                    ## define the set function to assign new 
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        
  }
  get <- function() x                     
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## this to return to operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
 