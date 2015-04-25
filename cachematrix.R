
## The aim of this function is to cache the result of the 'matrix inversion'
## operation in order to save its costly computation time in iterative 
## work on the same data set.
##     
## The function makeCacheMatrix creates a list containing a set of functions
## that handle the setting and retrieving of data together with its functions to
## handle the matrix inversion cacheing.
## The function assumes that the given matrix is always invertible. 

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the variables when the object is first defined
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    ## Define a list of functions to handle the given matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function tries to retrieve the "cached" inversed matrix value from 
## the environment of the "makeCacheMatrix" function. If the value does
## not exist (either because it is "null" from the first iteration or because of a new "set" request)
## the Matrix inverse is calculated and set in the "cache" environment.
cacheSolve <- function(x, ...) {
## Try to retrieve the stored value of the inverted matrix
        inv <- x$getinv()
    ## If "inv" is not empty it returns the value of the "cached" inverted matrix    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If the stored "inv" is empty, it calculates the inverse matrix and sets its value in the cache
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    ## Returns a matrix that is the inverse of the given matrix
    inv
}
