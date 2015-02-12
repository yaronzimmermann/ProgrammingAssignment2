## makeCacheMatrix and cacheSolve may save computation power by creating
## a matrix that can be stored in the cache, inversing it and retrieving
## it back from the cache without recalculation whenever needed. 

## This function creates a matrix object that can cache
## its inverse.

 makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
         set <- function(y) {
             x <<- y
             i <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) i <<- inverse
         getinverse <- function() i
         list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix object returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the inverse is retrieved
## from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
