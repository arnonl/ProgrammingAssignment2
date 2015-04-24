
## The aim of this function is to cache the result of 'Matrix inversion'
## operation in order to save its costly computation time in an iterative 
## work with the same data set.
## 
## The function, makeCacheMatrix creates a list containing a set of functions
## to handell setting and retriving of the data togather with functions to
## handell the Matrix inversion caching 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function try to retrive the "cached" inversed matrix value from 
## the environment of the "makeCacheMatrix" function, if the value is
## not exist ("null" because of the first iteration or because of a new "set")
## the Matrix inverse is calculated and set in the "cache" environment.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
