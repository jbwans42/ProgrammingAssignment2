## The two functions herein, makeCacheMatrix and cacheSolve, create 
## a "matrix" object that can cache its inverse, and retrieve the 
## inverse from the cache if it exists, or cause it to be calculated
## and cached.

## makeCacheMatrix - Takes an invertable matrix as input and returns
## a "matrix" object, actually a list, which can cache and retrieve 
## its inverse. The list contains methods to store and retrieve the
## original matrix and to store and retrieve the inverse of that 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the variable to store the inverse to NULL so as to
  ## be able to determine if the inverse is cached or not.
    v <- NULL 
  ## Define method to set the value of the matrix to be inverted
    set <- function(y) {
      x <<- y
      v <<- NULL
    }
  ## Define method to retrieve the original matrix
    get <- function()x
  ## Define method to cache the inverse matrix
    setsolvedinv <- function(solve) v <<- solve
  ## Define method to retrieve the inverse matrix
    getsolvedinv <- function() v
  ## Create and return the "cacheable matrix" list object
    list(set = set, get = get, 
         setsolvedinv = setsolvedinv, getsolvedinv = getsolvedinv)
}



## cacheSolve - Takes a "cacheable matrix" object as created by
## makeCacheMatrix and returns the inverse from the cache if it
## exists in the cache, or if it does not exist in the cache causes
##the inverse to be computed and cached before returning it.

cacheSolve <- function(x, ...) {
## Retrieve and return the inverse of x from the cache if it exists.
  v <- x$getsolvedinv()
  if (!is.null(v)){
    message("Retrieving cached data")
    return(v)
  }
## Retrieve the original matrix, and compute the inverse
  data <- x$get()
  v <- solve(data)
## Cache the inverse matrix, display the result and return
  x$setsolvedinv(v)
  v
}

