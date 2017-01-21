## Constructor function for creating a list of functions which
## support caching the inverse of a matrix, such that it
## does not need be recomputed on each acccess. You may set
## and get the actual matrix whose inverse should be computed
## via the set/get functions while the inverse can be accessed
## via the getInverse function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x   <<- m
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set        = set,
       get        = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## A modified version of the solve function which expects 
## a list created by the makeCacheMatrix function and uses
## it to retrieve the cached inverse of a matrix. If the
## inverse hasn't been computed yet, it will do so and 
## cache the result for future access.
## 
## NOTE: This function implicitly assumes that a given matrix
##       is invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
