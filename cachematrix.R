## Programming Assignment 2 - R Programming course
## These functions create a special "matrix" type object that can cache
## its inverse and also computes the inverse of one of these "matrix" type
## objects checking first if the inverse is cached.

## The functions are commented by following the guidelines from the
## Google's R Style Guide.
## https://google.github.io/styleguide/Rguide.xml

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special "matrix" object whose inverse can be cached.
  ##
  ## Args:
  ##   x: Is a conventional matrix from R. It is assumed that used matrices
  ##      within this function are invertible.
  ##
  ## Returns:
  ##   A special matrix type object which is really a list containing
  ##   different setter and getter functions for the matrix and its
  ##   inverse.
  
  inv <- NULL
  ## This function set the value of the matrix to y and the inverse to NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  ## This function allows to retrieve the matrix
  get <- function() {
    x
  }
  ## This function set the inverse of the matrix to inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  ## This function allows us to retrieve the inverse matrix
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Returns the inverse matrix of x. If the inverse has already been
  ## calculated it returns the cached inverse. The argument has to be one of
  ## special matrix types created by makeCacheMatrix.
  ##
  ## Args:
  ##   x: Special matrix type object created by makeCacheMatrix.
  ##   ...: Extra arguments for the function solve.
  ##
  ## Returns:
  ##   The inverse of x
  
  ## Look for the value within the matrix object x
  inv <- x$getInverse()
  ## If the inverse has already been calculated it returns it right away
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  ## If not, the inverse is calculated and returned
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
