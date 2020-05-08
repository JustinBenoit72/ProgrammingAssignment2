## makeCacheMatrix is used to create a list of functions that support cacheing.
## CacheSolve will check the cache of such a list and return the cached inverse
## matrix, if it exists, or calculate the inverse of the matrix.

## Create a list of four functions to cache an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y){
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(matrix_inv) m_inv <<- matrix_inv
    getmatrixinverse <- function() m_inv
    list(set = set, 
         get = get, 
         setmatrixinverse = setmatrixinverse, 
         getmatrixinverse = getmatrixinverse)
}


## CacheSolve function takes a list of four functions created by the makeCacheMatrix
## function and returns the cached inverse matrix if it exists, or calculates the
## inverse matrix and returns that if the cached version doesn't exist.
## The ... argument can be used to pass parameters to the solve function
## which is used to calculate the inverse matrix.
## Matrix data in x is assumed to be an invertible matrix.

cacheSolve <- function(x, ...) {
    m_inv <- x$getmatrixinverse()
    if(!is.null(m_inv)){
        message("getting cached data")
        return(m_inv)
    }
    mat <- x$get()
    m_inv <- solve(mat, ...)
    x$setmatrixinverse(m_inv)
    m_inv
}
