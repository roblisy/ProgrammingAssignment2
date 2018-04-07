## Programming Assignment week 2
## Caching the inverse of a matrix


## makeCacheMatrix creates a cachable "matrix" which can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to NULL (there isn't one to start..)
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # the get function returns the object, unaltered.0
  get <- function() {x}
  
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix returned in makeCacheMatrix()
## if the inverse already exists and the matrix hasn't changed, cacheSolve should retreive
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (is.null(inv) == FALSE) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# make a little testing...
test_matrix <- makeCacheMatrix(matrix(data = c(1,2,3,4), 
                                      nrow = 2, 
                                      ncol = 2, 
                                      byrow = TRUE))
## below should solve the inverse [-2,1,1.5,-.5] the first time, 
## but return the cache the second time (and print the 'getting cached data' line)
# display the matrix
head(matrix(data = 1:4, nrow = 2, ncol = 2, byrow = TRUE))

# try the function.
cacheSolve(test_matrix)
# try a 2nd time (should use cache)
cacheSolve(test_matrix)

# viola.




