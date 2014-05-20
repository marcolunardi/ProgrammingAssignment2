## My functions return the inverse of a matrix using the solve() function,
## and retrieving the result directly from cache if it's already in it.

## makeCacheMatrix is a function made of a list of named sub-functions;
## these named sub-functions provide all actions needed to get the inverse of
## a matrix, and to instantiate it into cache.

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y)  {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) invmatrix <<- solve
        getsolve <- function() invmatrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function checks whether the result needed is already in cache or not.
## If the result is already in cache, it return the message "getting cached data"
## and retrieves the result from cache, otherwise it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        invmatrix <- x$getsolve()
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setsolve(invmatrix)
        invmatrix
}
