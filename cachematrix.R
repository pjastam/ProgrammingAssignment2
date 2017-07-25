## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. We have written a pair of functions that cache the inverse 
## of any square invertible matrix. Your first call should be to the
## function makeCacheMatrix, your second to the function cacheSolve.

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse matrix 'm' as an empty matrix
  m <- NULL
  #Create a set function for 'x' and 'm'
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Create a simple function to get the input matrix 'x'
  get <- function() x
  #Create a function to set 'm' to a new, not yet cached inverse matrix
  setinverse <- function(inverse) m <<- inverse
  #Create a simple function to get the cached inverse 'm'
  getinverse <- function() m
  #Return a named list with 'set', 'get', 'setinverse' and 'getinverse' as arguments
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Get the inverse matrix from the cache
  m <- x$getinverse()
  #If a non-empty inverse matrix is returned from the cache, then use this cached inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If the cache returned an empty inverse matirx, then we continue with setting 'data' to the input matrix 'x'
  data <- x$get()
  #Calculate the matrix inverse 'm', assuming that the input matrix is square and invertible
  m <- solve(data, ...)
  #Update the cache by replacing it with the new inverse matrix
  x$setinverse(m)
  #Return the inverse matrix 'm'
  m
}
