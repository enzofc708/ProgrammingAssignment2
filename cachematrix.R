## These functions take a invertible matrix as input and return the
## inverse matrix in a more efficient way by cacheing their values. 

## The first function "makeCacheMatrix" converts a given matrix to a format
## that allows it to be cached. In more details, it takes a matrix "X" and
## assign 4 methods to it: "get", wich return its value; "set", which alters
## its value; "setinv", which alters the cached inverse matrix; and "getinv",
## which return the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted) inv <<- inverted
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The last function, "cacheSolve", takes an matrix as input and checks for
## its result in the cache. If it hasn't been previously calculated, then it
## calls the "solve" function to do the job.

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
