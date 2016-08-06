# This function caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL # this holds the cached value or NULL if nothing is cached
  set <- function(y) { # this stores a matrix
    x <<- y
    cache <<- NULL
  }
  get <- function() x # return the stored matrix
  setInverseMatrix <- function(inverse) cache <<- inverse # cache the input argument
  getInverseMatrix <- function() cache # get the cached value
  list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix) # return a list
}

# This function computes the inverse of a matrix created by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inverse < x$getInverseMatrix() # get the cached value
  if(!is.null(inverse) { # if cached value exists, return it
    message("loading cached data")
    return(inverse)
  }
  cachedData <- x$get() # otherwise, get the matrix, compute the inverse and store in cache
  inverse <- solve(cachedData, ...)
  x$setInverseMatrix(inverse)
  inverse # return the inverse
}
