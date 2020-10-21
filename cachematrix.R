## Put comments here that give an overall description of what your
## functions do

## The function below creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmat <- function(inverse) inv <<- inverse
  getmat <- function() inv
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
  inv <- x$getmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmat(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
}