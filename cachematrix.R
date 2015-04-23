## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix is a function that creates a list that contains 
#four different functions that will be utilized in the cacheSolve function:
# 1) set            set the value of a matrix
# 2) get            get the value of a matrix
# 3) setInverse     set the inverse of the matrix
# 4) getInverse     get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Write a short comment describing this function
# When the cacheSolve function is first ran, it will attempt to retrieve the inverse of the matrix via
# the getInverse() function, but the retrieved value will be NULL, as the matrix has not been solved yet.
# The function will then continue with $get() matrix retrieval part, solve() to obtain the inverse part, and finally
# the $setInverse() part to store this inversed matrix value.
# From now on, everytime the cacheSolve() function is called once more, the $getInverse() function will retrieve the
# inverse matrix, thus the repeated use of the solve() function can be avoided.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}
