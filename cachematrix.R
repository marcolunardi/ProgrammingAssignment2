## My functions return the inverse of a matrix using the solve() function,
## and retrieve the result directly from cache if it's already in it.

## makeCacheMatrix is a function made of a list of named sub-functions,
## and it works like an object does through its methods.
## These named sub-functions provide all actions needed to instantiate
## into cache the inverse of a matrix.

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


## cacheSolve function checks whether the needed inverse of the matrix 
## is already in cache or not.
## If it's already in cache, it return the message "getting cached data",
## and then it retrieves and returns the result from cache;
## otherwise it computes and returns the inverse of the matrix.

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
