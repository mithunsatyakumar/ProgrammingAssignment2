## This program is a set of functions that cache and compute the inverse of a matrix.

CreateCacheMatrix <- function(mtrx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtrx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtrx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);

    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function validates inverse of the "matrix" returned by `CreateCacheMatrix` above.
## If Inverse already been calculated (and the matrix has not changed), then
## `MycacheSolve` will retrieve the inverse from the cache.

MycacheSolve <- function(mtrx, ...) {
    inverse <- mtrx$getinv()
    if(!is.null(inverse)) {
        message("Fetching the data from cache...")
        return(inverse)
    }
    data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverse)
    return(inverse)
}
