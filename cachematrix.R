## This script contains two functions, makeCacheMatrix and cacheSolve, with these functions a matrix and its inverse
## is calculated and cached for future calculations

## makeCacheMatrix is a function that allows cache the value of a matrix and its inverse, it has four functions
##                  set(x) - set matrix value
##                  get() - acces to matrix value
##                  setInverse(y) - set inverse of matrix
##                  getInverse(y) - get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Create a function to set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL ## delete inverse in case the matrix change
    }
    ## Create a function to get the value of the matrix
    get <- function() x
    ## Create a function to set the value of the inverse of a matrix
    setInverse <- function(inverse) i <<- inverse
    ## Create a function to get the value of the inverse of a matrix
    getInverse <- function() i
    ## Return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##            If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
##            should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Check if the inverse of x is cached    
    i <- x$getInverse()
    ## If the inverse is cached return its value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## otherwise calulate it and cache the value
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}