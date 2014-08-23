## These functions describe the creation of the CacheMatrix data object, which allows for
## more efficient inverse matrix recall by caching the matrix inverse instead of re-calculating
## the matrix inverse.

## makeCacheMatrix(matrix)
## This function accepts an object of type matrix as the sole parameter.
## It returns an object of type CacheMatrix with the following methods/properties:
## $set - allows a redefinition of the internal matrix state. Clears the inverse.
## $inverse - The stored inverse of the matrix. Contains NULL if inverse not calculated.
## $getInverse - Retrieves calculated inverse of matrix. Returns NULL if inverse not calculated.
## $setInverse - Calculates and stores inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(CacheMatrix)
## This function accepts an object of type CacheMatrix as the sole parameter.
## If the CacheMatrix has an inverse already calculated, that value will be returned.
## If CacheMatrix does not already have an inverse calculated and stored, the calculation 
## and storage will be performed.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Retrieving cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
