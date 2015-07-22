## The following functions are used to store a matrix and
## then to solve the inverse matrix of the stored matrix.

## the 'makeCacheMatrix' function below creates a cache  
## matrix in the form of a list that allows set and get
## the value for the cached matrix and its inverse matrix.

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
## of the cached matrix from the function 'makeCacheMatrix'
## by first retrieving the inverse matrix if it has already
## been calculated or optionally solves the inverse matrix
## and caches the results if the inverse matrix was not
## previously cached.

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
