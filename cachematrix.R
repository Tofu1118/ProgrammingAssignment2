## The following two function uses the '<<-' R operator to create a matrix object
## that can cache the values of its inverse to expedite repeated calculations.
## The functions assume that the matrix in question is invertible.

## makeCacheMatrix is a function that creates a cached matrix object based on a 
## matrix argument with ability to set and get its matrix and matrix inverse.

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


## cacheSolve returns the inverse of the cached matrix object. 
## It will retrieved the cached inverse if it exists, else it calculates and returns 
## the inverse using solve() while also caching the value in the cached matrix 
## object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse ()
    if (!is.null(i)) {
        message("Getting cached data")
        return (i)
    }
    matrix <- x$get ()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}
