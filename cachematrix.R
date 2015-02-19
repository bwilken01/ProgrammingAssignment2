## Below is a pair of functions that cache the inverse of a matrix, i.e. a time-consuming computation.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #Set value of matrix
    x <<- y #x is matrix
    inv <<- NULL #initialize inv to null
  }
  get <- function() x #Get value of matrix
  setinverse <- function(inverse) inv <<- inverse #Set inv to value of inverse from cacheSolve function
  getinverse <- function() inv #Get value of inverse by calling inv
  list(set = set, get = get, #Return list of functions when makeCacheMatrix is called
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
###If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #if inv is empty, get matrix 'x'
  inv <- solve(data, ...) #Computing the inverse of a square matrix can be done with the solve function in R
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
