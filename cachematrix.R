## Calculates the inverse of a matrix and cache the result for later use
## Usage: 1 - x<-makeCacheMatrix(A)
##        2 - cacheSolve(x)

##  creates a special "vector", containing a function to
##      set the Matrix
##      get the Matrix
##      set the Inverse matrix
##      get the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invx) m <<- invx
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of the matrix stored in the above vector. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the resulting matrix
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
