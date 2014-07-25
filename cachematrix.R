## The functions below cache a matrix and return its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  ## caches a matrix, calculates its  
  ## inverse, returns both in a list
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- solve(x) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## calculates matrix inverse 
  ## (if not done) and returns it.
  i <- x$getinv()
  j <- x$get()
  
  if(isTRUE(all.equal(x$get(), j)) == TRUE) {
    message("getting cached data")
    return(i)
  }
  
  message("the matrix has changed")
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
