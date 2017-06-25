## This function creates a special "matrix" object that can cache its inverse.

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

