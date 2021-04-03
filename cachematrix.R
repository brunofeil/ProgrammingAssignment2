## The function makeCacheMatrix get as argument a matrix class object, create a 
## "cached" matrix through a set and a get function and store its inverse also through
## a set and a get function.
## The function cacheSolve gets as argument a "cached" matrix and returns its
## inverse. If the inverse of the "cached" matrix exists (it is already calculated),
## then return the cached inverse of the matrix. If it not exists, then calculate its
## inverse, set the inverse to the "cached" matrix and return it

## Get a matrix and create a structure to "cache" the matrix and its inverse. Return
## the setters and getters to the cached matrix and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverted_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverted_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverted_matrix <<- solve
    getinverse <- function() inverted_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the inverse of the "cached" matrix. If the inverse already exists and the
## matrix has not changed, return the cached inverse. If it not exists or the matrix
## has changed, calculate the inverse, set it to the "cached" matrix and return it 
cacheSolve <- function(x, ...) {
    inverted_matrix <- x$getinverse()
    if(!is.null(inverted_matrix)) {
        message("getting cached data")
        return(inverted_matrix)
    }
    data <- x$get()
    inverted_matrix <- solve(data, ...)
    x$setinverse(inverted_matrix)
    inverted_matrix
}