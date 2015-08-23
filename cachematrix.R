## The two functions below are creating a cache of matrices and their inverts and are used 
## to save on computational effort. The first function stores matrices and their inverts, 
## so when the second one is called, it will first check whether the invert is available as
## a cached matrix. If not it will compute it and store it in the cached data. 

## NOTE: The code for the 2 functions is heavily based on sample code provided by the 
## instructors of the coursera class "R Programming" for the calculation of cached mean.

## This function is a list of "subfunctions" that store and return your input matrices 
## and their inverts (in a way, creating the cached matrix).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invert) inv <<- invert
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This functions returns the invert of your matrix either by (i) returning it as a cached matrix
## from the function makeCacheMatrix or (ii) by calculating it from scratch, if it has not been 
## calculated before

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
        ## If the invert of your matrix has been calculated before it will be returned from the 
        ## cached data without recalculation
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
        ## If yor matrix is new, the function will calculate and return its invert here and 
        ## will store it in the cached data 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
