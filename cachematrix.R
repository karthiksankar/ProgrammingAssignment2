## Caching the Inverse of a Matrix

## This function provides the following functions:
##
## set(): Caches the input matrix
## get(): Retrieves the input matrix
## setInverse(): Caches the inverse matrix
## getInverse(): Retrieves the inverse matrix
##
makeCacheMatrix <- function(cachedMatrix = matrix()) {
  cachedInverse <- NULL
  set <- function(initialMatrix) {
    cachedMatrix <<- initialMatrix
    cachedInverse <<- NULL
  }
  
  get <- function()
    cachedMatrix
  
  setInverse <- function(inverseMatrix)
    cachedInverse <<- inverseMatrix
  
  getInverse <- function()
    cachedInverse
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function first attempts to get the inverse of the input matrix from the cache
## If the inverse is already cached, it is retrieved from the cache and returned
## If the inverse is not already cached, it is computed and stored in the cache
##
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  
  return(inverseMatrix)
}
