## Functions used to extend the functionality of a matrix to
## calculate its inverse and cache it (so it doesn't have to be
## computed again if the inverse is requiered further ahead)

## Creates a "special" matrix that stores a numeric matrix and provides 
## functionality to cache its inverse from another function. The function
## assumes that the matrix is invertible and that the cached inverse is indeed
## the inverse of the matrix (i.e. it will not compare the provided matrix to a
## calculated inverse of the stored matrix)
makeCacheMatrix <- function(x = matrix()) {
  ##variable to store the inverse of the matrix
  inv <- NULL
  ##if the matrix is reset, make the cached inverse null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Return the matrix
  get <- function() x
  ## Set the inverse
  setinverse <- function(solve) inv <<- solve
  ##Get the inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves (calculates the invert of) the provided cacheMatrix by
## calling the solve() function, and takes advantatge of cacheMatrix's capabilities
## to cache the inverse matrix in the provided cacheMatrix. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the inverse of the provided cacheMatrix
  inv <- x$getinverse()
  ## If an inverse was returned, return it and exit the method
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##...else, get the original matrix and call solve to obtain the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  ##... finally, cache the inverse and return it
  x$setinverse(inv)
  inv  
}