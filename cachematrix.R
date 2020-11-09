## The first function creates a matrix and also calculates its inverse. To do so, it makes 
## use of lexical scoping whereby the object m gets a null val and can be modified throughout the 
## function execution.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y=4) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function checks if the value of the matrix inverse is present in the cache. 
## If not, it calculates in and stores in in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  m
}
