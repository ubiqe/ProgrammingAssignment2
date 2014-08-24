## The following two functions can be used to compute the inverse of a matrix
## and store the results in "cache"

## makeCacheMatrix takes an invertible matrix as a parameter and creates a set
## of 4 functions to operate on it: get & set to retrieve and write new data
## matrix, respectively; and getinverse & setinverse to retrieve and store the
## result of inversion

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a list of functions on a matrix as a parameter and
## computes the inverse or returns a cached result, if the inverse has already
## been computed

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
