## A pair of functions that are used to create a special object that
## stores a square matrix and its inverted one with the possibility to
## cache it.


## Create an object which stores a square matrix (input x) and its
## inverted matrix. There are set/get functions for the both matrices.

makeCacheMatrix <- function(x = matrix()) {
    ## Create an object storing the input matrix 'x'.
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseM) inv <<- inverseM
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculate inverted matrix to the input one. Or alternatively get
## cached matrix if already calculated before and the input matrix has
## not changed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the invert of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
