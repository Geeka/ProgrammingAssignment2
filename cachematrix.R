##Cache the inverse of matrix using lexical scoping of R 
##Example usage
##> y<-matrix(rnorm(9),3,3)
##> z<-makeCacheMatrix(y)
##> cacheSolve(z) - first time calculates the inverse
##> cacheSolve(z) - second time cached data is returned

## makes a special matrix which is created in the environment
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## solve function calculates and returns the inverse. 
## If inverse is already present in the environment 
## and the value of the special matrix is not changed
## returns cached data
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
