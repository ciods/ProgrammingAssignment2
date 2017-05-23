## Create a pair of functions to cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setinv <- function(mx_inv) mx <<- mx_inv
    getinv <- function() mx

    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mx <- x$getinv()
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }

    data <- x$get()
    mx <- solve(data, ...)
    x$setinv(mx)
    mx
}
