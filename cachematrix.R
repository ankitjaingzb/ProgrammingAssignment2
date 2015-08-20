## These functions provide functionality to compute and cache the inverse of a matrix.
## Users should use the list object returned by makeCacheMatrix to create a matrix object.
## Subsequent changes to the matrix should be done by the set function on makeCacheMatrix.
## Inorder to obtain the inverse the makeCacheMatrix object should be passed to the 
## cacheSolve function. An example of the usage can be seen in testCacheMatrix.

## This function creates a special "matrix" object that can cache its inverse.
## It provides helper functions to get, set the matrix and get, set the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above and caches it. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## This function tests the functionality of the above two functions.
## It checks if the inverse in the second time is retrieved from cache which is known by
## the diagonastic message "getting cached data"
## It also checks that when the matrix is changed before asking for the inverse third time
## then the results should not be computed from the cache.
testCacheMatrix <- function()
{
  ## Create two matrix
  m1 <- matrix(c(1,2,3,4),nrow=2, ncol=2)
  m2 <- matrix(c(4,3,2,1),nrow=2, ncol=2)
  ## Make a cache matrix object from the first
  m1Cache <- makeCacheMatrix(m1)

  print("Now matrix is")
  print(m1Cache$get())
  ## Ask for the solution two times
  print("Getting inverse first time")
  print(cacheSolve(m1Cache))
  print("Getting inverse second time")
  print(cacheSolve(m1Cache))
  
  ## Set the matrix again
  m1Cache$set(m2)
  print("Now matrix is changed to")
  print(m1Cache$get())
  print("Getting inverse third time")
  print(cacheSolve(m1Cache))
}