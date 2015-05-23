## Put comments here that give an overall description of what your
## functions do

## make inversable matrix

makeCacheMatrix <- function(x = matrix()) { 
  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) value <<- solve
  getinverse <- function() value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## inverse matrix, if matrix is inversed already, return the cached version, 
## else, inverse the matrix and save it in cache and return 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  value <- x$getinverse()
  
  if(!is.null(value)) {
    message("getting cached matrix")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setinverse(value)
  value
}
