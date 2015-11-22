## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : Creates a matrix object that can cache its own inverse matrix.
##                   Returns a list of functions to create the matrix, retrieve
##                   the matrix, create the inverse matrix, and retrieve the inverse
##                   matrix.

makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        
        get <- function() x
        
        setInverse = function(inverse)  xInverse <<- inverse
        
        getInverse = function() xInverse
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve : Computes the inverse of the matrix returned by makeCacheMatrix
##              If the matrix has not changed, retrieve the inverse from the cache,
##              rather than computing the inverse again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        xInverse <- x$getInverse()
        
        if(!is.null(xInverse)){
              message("getting cached data")
              return(xInverse)
        }
        
        data <- x$get()
        
        xInverse <- solve(data)

        x$setInverse(xInverse)

        xInverse
}
