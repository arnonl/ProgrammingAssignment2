
## The aim of this function is to chace the result of 'Matrix inversion'
## operation in order to save its costly computation time in an iterative 
## work with the same data set.
## 
## Write a short comment describing this function
## The function, makeCacheMatrix creates a list containing a set of functions
## to handell setting and retriving of the data togather with functions to
## handell the Matrix inversion caching 


## This function get try to retrive the "cached" inversed matrix value from 
## the the environment of where the x variable was defined, if the value is
## not exist the Matrix inverse is calculated and set.
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


## Write a short comment describing this function

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
