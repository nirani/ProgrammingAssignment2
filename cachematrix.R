## The functions compute the inverse of a given matrix and cache the solution, 
## so that when called another time, the computation does not have to be
## performed again.

## This function creates a framework to store a given matrix and its calculated
## inverse. It returns a list of functions to:
## set the current matrix
## get the current matrix
## set the inverse of the current matrix
## get the inverse of the current matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns a matrix that is the inverse of "x"
## It first determines, whether the inverse for "x" is already calculated
## and in such case it returns that value. If it is not, then it calculates the
## inverse and stores that value.
cacheSolve <- function(x, ...) {

  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
