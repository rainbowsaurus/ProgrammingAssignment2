## This pair of functions will help to cache the inverse of a matrix

## makeCacheMatrix is a that returns a list of functions for set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return a matrix that is the inverse of 'x'
## If the inverse is cached, it will return it, 
## otherwise it will calculate the inverse, cache it, and return it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    ## if i is not null, it will return the cached data
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## if i is not cached, it will set the inverse and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
