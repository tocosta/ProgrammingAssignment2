## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## This pair of functions provides the ability to cache the inverse of a matrix.

## Function makeCacheMatrix: 
## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Return a list of functions that can get and set the matrix and its inverse
    
    ## declaration of the inverse
    inverse <- NULL
    
    ## function set: sets the original matrix and resets its inverse cached
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## function get: returns the original matrix
    get <- function() x
    
    ## function setinverse: sets the cached inverse of the original matrix
    setinverse <- function(i) inverse <<- i
    
    ## function getinverse: returns the cached inverse of the original matrix
    getinverse <- function() inverse
    
    ## the return
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve:
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## gets the inverse from the cache of the original matrix
    inverse <- x$getinverse()
    
    ## if there is cache, then returns the cached computed inverse 
    ## and prints a message
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## otherwise it will get the original matrix and compute its inverse
    data <- x$get()
    inverse <- solve(data, ...)
    
    ## sets the computed inverse to the cache
    x$setinverse(inverse)
    
    ## returns the inverse
    inverse
}
