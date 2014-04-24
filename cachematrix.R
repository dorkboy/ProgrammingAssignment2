## Matrix inversion can be a costly computation.  The following functions attempt
## to use the benefit of caching the inverse of a matrix rather rather than 
## compute it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    set <- function(y)  {
       x <<- y
       x_inverse <<- NULL
    }

    get <-function() x
    setinverse <- function(inverse) x_inverse <<- inverse
    getinverse <- function() x_inverse

    list(set = set, get = get,
         setinverse = setinverse,
	 getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" function 
## returned by makeCacheMatrix() above.  If the inverse has already been calculated
## (and the matrix has not been changed), then the cacheSolve() function should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$getinverse()   

    if (!is.null(x_inverse)) {
        message("Getting cached inverse of matrix x")
	return(x_inverse)
    } 

    x_inverse <- solve(x$get())
    x$setinverse(x_inverse)

    x_inverse
}
