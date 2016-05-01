## makeCacheMatrix() creates a special matrix object.
## cacheSolve() calculates the inverse of the matrix
## and returns it.

## Accepts a square invertible matrix as input X and returns a list
## which is a cached function for use in cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setinv = function(inverse) m <<- inverse 
    getinv = function() m
    list(set=set, 
         get=get, 
         setinv=setinv, 
         getinv=getinv)
}

## Accepts the cached function in makeCacheMatrix() as input x. If
## the inverse has already been calculated it is returned. If not 
## the inverse matrix is calculated and returned.

cacheSolve <- function(x, ...) {
    m = x$getinv()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data, ...)
    x$setinv(m)
    return(m)
}
