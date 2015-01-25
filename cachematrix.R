## In this code a function makeCacheMatrix is defined this function stores a matrix and cached value of inverse of matrix
## Initially value of cache is set to NULL

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    setmatrix <- function(new) {
        x <<- new
        cache <<- NULL
    }
    getmatrix <- function() x
    cacheinverse <- function(solve) cache <<- solve
    getinverse <- function() cache
    list(setmatrix = setmatrix, getmatrix = getmatrix, cacheinverse = cacheinverse, getinverse = getinverse)
}

cacheSolve <- function(x = matrix(), ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ndata <- x$getmatrix()
    inverse <- solve(ndata)
    x$cacheinverse(inverse)
    inverse
}
