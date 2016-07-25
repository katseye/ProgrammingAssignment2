## makeCacheMatrix creates a matrix object and computes its inverse matrix
## cacheSolve checks if the matrix has changed ior not. If it's the same, it fetches the inverse from the cache 
## rather than compute it again.

## makeCacheMatrix receives a matrix as an input argument and creates a cache for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x

  ## creates an inverse of the matrix passed to it  
  setinverse <- function(solve) m <<- solve

  ## fetches the inverse from the cache  
  getinverse <- function() m

  ## creates a list of the created and fetched inverse matrices  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the "matrix" returned by makeCacheMatrix(). 
## If the inverse already exists and the matrix has not changed, it'll fetch 
## the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  
  ##get the inverse matrix from the cache if the cache is not empty
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Create inverse matrix and set the value in the cache if the matrix has changed
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
