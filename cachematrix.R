
## makeCacheMatrix 
## create an object which wraps the passed in matrix and provides 
## a field and getters/setters for the inverse
#
## parm x - a matrix to encapsulate or wrap
## returns the wrapped object encapulating the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## returns the inverse of the matrix passed in.
## if the inverse has already been calculated then it is returned from the "cache"
## otherwise it is computed and saved into the "cache"
#
## parm x - a matrix whose inverse we want
## returns the inverse of the passed in matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
