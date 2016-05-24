## This source file contains two functions:
## The first function shall create a cache matrix
## The second function shall calculate the inverse of the stored matrix or return the cached matrix if already calculated

## Function to create matrix and respective functions

makeCacheMatrix <- function(x = matrix()) {
  # inv set to NULL to ensure inverse is calcualted on next cache colve call
  inv <- NULL
  
  # Function to allow matrix to be changed, on new call inv set to NULL to ensure inverse is calcualted on next cache solve call
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to return original matrix 
  get <- function() x
  
  # Function to set inverse matrix 
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to return inverse matrix 
  getinverse <- function() inv
  
  # List of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Function shall calculate the inverse of the matrix if not already calculated, otherwise will return cached inverse.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  #Check if inv has been calculated and cached
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  # Get original matrix
  data <- x$get()
  
  #solve for inverse of matrix
  inv <- solve(data, ...)
  #set inverse in special matrix value
  x$setinv(inv)
  #return inverse matrix
  inv
}
