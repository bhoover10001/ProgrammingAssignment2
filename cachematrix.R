## Since solving a matrix can be time intensive, this routine
## Will determine if the solution has already been created and return
## that.  If not, it will put the matrix solve solution into the cache

## checks to see if the matrix is in the cache.  If it isn't, puts 
## it into the cache and calculates the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get=get, 
      setinverse = setinverse,
      getinverse = getinverse)
}


## Determines if x, which is from makeCacheMatrix already has an inverse.  If so, 
## returns the cached version. Else calculates it

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
