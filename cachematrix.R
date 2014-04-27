## These functions have been created to answer Programming Assignment 2
## of the John Hopkins R Programming course on Coursera.

## These functions solve the inverse of a giving matrix "x".
## To minimise the time to do this, the functions cache and recall the
## inverse when the matrix has not changed.

## The 1st function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The 2nd function returns the inverse matrix of the given matrix "x".
## It either takes it from cache if it has previously been calculated
## or calculates it using setinv.

cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
