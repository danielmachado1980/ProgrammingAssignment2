## Put comments here that give an overall description of what your
## functions do

## here create the matrix
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

## here calculate the inverse matrix while cache it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(is.atomic(x)) { 
    inv <- solve(x)
  }  
  else  {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }
  inv
}
