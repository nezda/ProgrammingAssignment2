## Factory function makeCacheMatrix produces a wrapper object around a matrix
## that provides special inverse memoizing capability.  Paired function
## cacheSolve serves to access the inverse of one of these special wrapped
## matrix objects, computing it on demand.

# Creates object that wraps numeric argument matrix x with get/set methods that
# intercept modifications so that a memoized inverse can be maintained
# correctly.  The inverse of the matrix should be computed with function
# cacheSolve and can be accessed by either that function or object method
# getinverse.
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedInverse <<- inverse
        getinverse <- function() cachedInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Returns a matrix that is the inverse of 'x' (which must be a special "matrix"
# created with makeCacheMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachedInverse <- x$getinverse()
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                return(cachedInverse)
        }
        data <- x$get()
        cachedInverse <- solve(data, ...)
        x$setinverse(cachedInverse)
        cachedInverse
}
