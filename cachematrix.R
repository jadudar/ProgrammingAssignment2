
## makeCacheMatrix returns a vector of functions for
## interacting with a cached solution (inverted matrix)
## for the argument x.
##
## set(a) stores a as the matrix to be inverted
## get() returns the matrix to be inverted
## setinv() stores the cached inverse
## getniv() returns the cached inverse, or null if there is none.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix stored
## in the vector x. (The argument x is a vector as returned
## by makeCacheMatrix.) For efficiency, cacheSolve returns
## a cached value for
## the inverse if it is available, and if not, cacheSolve
## computes the inverse and stores it in the cache matrix
## for subsequent use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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
