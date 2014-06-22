## Description: This script creates a pair of functions to cache the inverse of a matrix
## assuming the given matrix is always inversible

## This function creates a special "matrix" object that can cache its inverse
## It provides four sub-functions, which can get and set the matrix, and get and set
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL ## inverse of the matrix
    ## set the matrix itself
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x ## get the matrix itself
    setinverse <- function(inverse) invs <<- inverse ## set the inverse
    getinverse <- function() invs ## get the inverse
    ## return the list of all four sub-functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the inverse is fetched directly from 
## the cache. Otherwise, the inverse is calculated and cached into the special
## "matrix" object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinverse() ## get the inverse of x
    ## if the inverse has already been cached, return it
    if (!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    else { ## otherwise, calculate and cache it
        data <- x$get() ## get the matrix
        invs <- solve(data, ...) ## calculate the inverse
        x$setinverse(invs) ## cache the inverse
        invs ## return the inverse
    }
}


