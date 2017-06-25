## This function creates a special "matrix" object that can cache its inverse.
## These functions allow the user to cache the inverse of a square matrix
## to save in computation time in not having to recalculate the inverse of
## a square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## cacheSolve returns the inverse of a square matrix by first checking if it
## already exists in the cache and using this object
## If not the inverse is calculated

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  ## Compute the inverse of a square matrix can be done with the  solve  function in R
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## To test, create 2 by 2 matrix
## Original_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## Original_matrix$get()
## cacheSolve(Original_matrix)

