## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix list object that can be utilized by the cacheSolve
## function.  It stores the matrix object and its inverse if the inverse has already been
## solved.  Otherwise it stores null so that cacheSolve will know that it needs to solve for
## the inverse.  This saves computing time for deterimining inverses by only requiring the inverse
## for a particular matrix to be determined once.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve utilizes the makeCacheMatrix list object.  It checks to see whether an inverse
## has already been computed and returns this inverse if it has.  Otherwise it computes the
## inverse and stores it in the makeCacheMatrix object for future reference.  It also returns a message
## letting the user know that it is retrieving the cached inverse if it already exists.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
