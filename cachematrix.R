## MakeCacheMatrix set and get a matrix value and of its inverse
## CacheSolve calculates the inverse of a matrix, but if it is already in the cache it gets the value

## This function creates a list that set and get the values of a matrix and of its inverse

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## This function first checks if the inverse of a matrix is stored in cache. If so it gets it, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}
