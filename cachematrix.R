
## makeCacheMatrix manages a matrix and returns a list of four
## functions for interacting with said matrix:
## set(x) sets the managed matrix
## get() returns the managed matrix
## getInverse() returns the inverse of the matrix (if it has been set)
## setInverse(x) sets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(newInverse) {
        inverse <<- newInverse 
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(set = set, get = get, 
         getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve returns the inverse of the matrix contained
## in object 'x'.  'x' is assumed to be an object created by
## by the makeCacheMatrix function.  If the inverse has not 
## yet been calcuated, it is calculated and cached, and subsequent
## calls to cacheSolve with the same object will return the cached
## inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(is.null(inverse)) {
        message("Creating cache...")    
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }

    inverse
}
