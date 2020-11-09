## The first function creates a matrix and also calculates its inverse. To do so, it makes 
## use of lexical scoping whereby the object m gets a null val and can be modified throughout the 
## function execution.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=4) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function checks if the value of the matrix inverse is present in the cache. 
## If not, it calculates in and stores in in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
