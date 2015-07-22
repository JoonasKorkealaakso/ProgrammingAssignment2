## The following functions stores a matrix in cache memory
## and then solves the inverse matrix of this cached matrix.

## the 'makeCacheMatrix' function below creates a special
## matrix in the form of a list which contains functions to
## set and get the values of the cache matrix and the values
## of its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
               setInv = setInv,
               getInv = getInv)
}

## The 'cacheSolve' function calculates the inverse matrix
## of the cache matrix created with the 'makeCacheMatrix'
## function. The 'cacheSolve' function first attempts to 
## retrieve the inverse matrix from the cache. If the inverse
## matrix has not been calculated the 'cacheSolve' function
## solves the inverse matrix and sets the inverse matrix in
## the cache with the setInv function.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInv(m)
        m
}