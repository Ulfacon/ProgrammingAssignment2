## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## There are two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                          ## initializing inverse as null
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x                ## function to get matrix x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i         ## function to obtain inverse of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This is used to get the cache data 

cacheSolve <- function(x, ...) {      ## gets cache data 
  i <- x$getinverse()
  if (!is.null(i)) {                  ## checking whether inverse is null 
    message("getting cached data")
    return(i)                         ## returns inverse value
  }
  data <- x$get()
  i <- solve(data, ...)               ## calculates inverse value
  x$setinverse(i)
  i
}
