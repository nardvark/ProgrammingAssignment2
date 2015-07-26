##makeCacheMatrix creates a list object that contains a function to set the 
##value of the matrix, get the value of the matrix, set the value of the matrix 
##inverse, and get the value of the matrix inverse. cacheSolve computes the
##inverse of a matrix and sets that value into the cache via setinv, unless it
##has already been computed, in which case it returns the cached value of the
##inverse.

## Function to set and get the value of input matrix, and set and get value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function(solve) inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to compute matrix inverse and place result in cache, unless it has
## been computed previously.  In that case the cached value is returned.

cacheSolve <- function(testobj, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- testobj$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  data <- testobj$get()
  inv <- solve(data, ...)
  testobj$setinv(inv)
  inv
}
