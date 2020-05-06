## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Explaination - 
## (1) Function that returns a list of 4 functions to set and get from cache
## the matrix of interest and its inverse. The arg 'x' is the matrix of interest
## that can be set directly when this function is called, but it can also be
## set or modified calling the function 'set_matrix' from the list returned. 
## The other three functions in the list will be used by the next function to:
## get_matrix = get the matrix from cache,
## set_inverse = set the inverse to cache after calculating it,
## get_inverse = get the inverse from cache if previously calculated.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
## Explaination - 
## (2) Function that returns the inverse of the matrix that was set
## using makeCacheMatrix(). 
## It first checks if it was previously calculated, and gets 
## the result from cache if it was. If it wasn't, it calculates it, 
## sets the result to cache and returns the result.
