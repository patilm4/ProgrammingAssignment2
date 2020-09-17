## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               ## Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}                 ## function to get matrix x
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() (inv)        ## function to obtain inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write to short comment to describing this function
## This is used to get the cache data
cachesolve <- function(x, ...){      ## get cache data
  inv <- x$getInverse()
  if(!is.null(inv)){                ## Checking whether inverse is null
    message("getting cached data")
    return(inv)                     ## return inverse value
  }
  mat <- x$get
  inv <- solve(mat, ...)            ## calculate inverse value
  x$setInverse(inv)
  inv
}