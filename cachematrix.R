## These two functions can create, invert and then cache any given (invertible)
## matrix, thus saving computation time. 

## This function creates a cached matrix and auxiliary functions to set and get
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function checks if there is a cached inverted matrix and returns its
## content. If there is no cached inverted matrix, the function calculates the 
## inverse of the matrix and stores the result.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}
