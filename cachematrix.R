## The function makeCacheMatrix creates a matrix object that can cache the inverse and
## cacheSolve computes the inverse of the matrix returned from makeCacheMatrix if the inverse 
## has yet to be calculated. Otherwise, returns the inverse from cache.

## Creates a matrix object that can cache the inverse by using a list containing a function
## to complete setting the value of the matrix, getting the value,
## setting the value of the inverse matrix, and getting the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to calculate the inverse of the matrix from makeCacheMatrix. If inverse was already calculated,
## the function gets the inverse from the cache. Otherwise, calculates the inverse and sets the value of the inverse
## in the cache by using setinverse function

cacheSolve <- function(x, ...) {
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinverse(invs)
    invs
}
