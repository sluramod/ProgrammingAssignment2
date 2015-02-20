## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Stores new value and resets cached inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Retrieves stored matrix
  get <- function() x
  ## Updates stored matrix inverse
  setSolve <- function(cacheSolve) m <<- cacheSolve
  ## Retrives stored matrix inverse
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Retrive cached value
  m <- x$getSolve()
  ## Something is in cache, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Nothing is in cache, calculating matrix inverse
  ## Retrive stored matrix
  data <- x$get()
  ## Inverse the matrix
  m <- solve(data, ...)
  ## Update cached value
  x$setSolve(m)
  m
}
