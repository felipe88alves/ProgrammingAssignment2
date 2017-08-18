## These function will calculate the Inverse of a matrix 
## After calculating the matrix's inverse, that value will be stored.
## If the matrix inverse calculation is called again, the stored value will be given.

## This function is responsible for getting and setting the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setSolve <- function(mean) inv <<- mean
        getSolve <- function() inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This functions returns the matrix inverse. Be it by calculation or by a getting a stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getSolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get())
        x$setSolve(inv)
        inv
}