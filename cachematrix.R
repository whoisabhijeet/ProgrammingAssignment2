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
}