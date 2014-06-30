## Since solving a matrix can be time intensive, this routine
## Will determine if the solution has already been created and return
## that.  If not, it will put the matrix solve solution into the cache

## invocation:
## testMatrix <- makeCacheMatrix(matrix(c(4, 7, 2, 6), nrow=2, ncol=2))
## cacheSolve(testMatrix)

## to update the matrix (and reset the cache)
## testMatrix$set(matrix(c(7, 4, 6, 2), nrow=2, ncol=2))

## Creates a special matrix type that has four functions,
## get <- returns the original matrix
## set <- sets the matrix and clears the cached inverse
## getinverse <- gets the inverse, if it was already calculated.
## setinverse <- sets the invesee and the cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get=get, 
      setinverse = setinverse,
      getinverse = getinverse)
}


## Determines if x, which is from makeCacheMatrix already has an inverse.  If so, 
## returns the cached version, otherwise, calculates the inverse and puts the inverse into 
## the cache before returning it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
