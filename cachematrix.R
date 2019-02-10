## Author : Adam Biskup
## Prog Assigment #2 - course Programming in R 
## Our cached matrix object is a list consisting of functions-interfaces to
## internal variables :
## matrix (x) variable and matrix (invres) variable which wil NULL or 
## containing result (matrix type) of inversion of x matrix  


## Creates list consisting of functions-interfaces and initializes its internal
## variable invres as NULL

makeCacheMatrix <- function(x = matrix()) {
    invres <- NULL
    set <- function(y) {
        x <<- y
        invres <<- NULL
    }
    get <- function() x
    setinvres <- function(inv) invres <<- inv
    getinvres <- function() invres
    
    list( get = get, set = set , getinvres = getinvres , setinvres = setinvres )
}


## Fumction solves inversion of matrix stroed inn x list - 
## if result is not computed yet and remebers its result in cache variable invres
## or simply returns cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    res <- x$getinvres()
    if( !is.null(res)) {
        print('Already computed in cached variable - returning cached value.')
        return(res)
    }
    print('Solving inversion and setting cached value.')
    res <- solve(x$get() , ...)
    ## Caching result
    x$setinvres(res)
    # Returning computed result
    res
}
