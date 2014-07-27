## This set of functions calculates the inverse of a matrix. Since this is
## a data intensive calculation the inverse of the matrix is cached. If
## the inverse is needed for a later calculation we first check the cache
## to see if the inverse has already been computed, and if so, use the
## cached inverse.

## The makeCacheMatrix function creares a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) myInverse <<- inverse
    getInverse <- function() myInverse
    list (set = set, get = get, 
          setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned
## by the above function makeCacheMatrix. If the inverse has already been 
## calculated in the above function, and the matrix has not changed, then
## the inverse from the cache is retrieved.

cacheSolve <- function(x, ...) {
    myInverse <- x$getInverse()
    if(!is.null(myInverse)) {
        message("getting cached data")
        return(myInverse)
    }
    data <- x$get()
    myInverse <- solve(data, ...)
    x$setInverse(myInverse)
    myInverse
}
