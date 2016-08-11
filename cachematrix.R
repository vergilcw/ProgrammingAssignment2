## This set of two functions provides a a framework for caching the inverse of
## a matrix along with the original matrix. This can be useful to prevent
## recalculating a matrix inverse multiple times.

## The first function makeCacheMatrix sets up the data storage object in which
## both the matrix and its inverse (if already calculated) are stored in a list
## and are accessible via functions called get() and getinv().

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function cacheSolve takes as its primary argument a data storage
## object created by makeCacheMatrix, along with optional other arguments (in
## ...) to be passed to the solve() function. It checks for presence of the
## cached inverse matrix and calls solve if no cached inverse matrix exists,
## storing the result in memory. It returns the value of the inverse of the
## original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m

}
