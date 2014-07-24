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
      setinv <- function(solve) i <<- solve
      getinv <- function() solve(x)
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x = matrix(), ...) {
      ## calculates matrix inverse 
      ## (if not done) and returns it.
      i <- x$getinv()
      j <- x$get()
      if(!is.null(i)) {
#            if(j == x) {
                  message("getting cached data")
                  return(i)  
#            }
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
