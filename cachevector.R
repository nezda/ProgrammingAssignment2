## Factory function makeVector produces a wrapper object around a vector
## that provides special mean memoizing capability.  Paired function
## cachemean serves to access the mean of one of these special wrapped
## vector objects, computing it on demand.

# Creates object that wraps numeric argument vector x with get/set methods that
# intercept modifications so that a memoized mean can be maintained
# correctly.  The mean of the vector should be computed with function
# cachemean and can be accessed by either that function or object method
# getmean
makeVector <- function(x = numeric()) {
        cachedMean <- NULL
        set <- function(y) {
                x <<- y
                cachedMean <<- NULL
        }
        get <- function() x
        setmean <- function(mean) cachedMean <<- mean
        getmean <- function() cachedMean
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# calculates the mean of the special "vector" created with makeVector
# memoizing the calculation
cachemean <- function(x, ...) {
        cachedMean <- x$getmean()
        if(!is.null(cachedMean)) {
                message("getting cached data")
                return(cachedMean)
        }
        data <- x$get()
        cachedMean <- mean(data, ...)
        x$setmean(cachedMean)
        cachedMean
}

# # explanatory usage
# rawVec = c(1, 2, 3)
# vec = makeVector(rawVec)
# identical(rawVec, vec$get()) # TRUE
# is.null(vec$getmean()) # TRUE - not calculated yet
# cachemean(vec) # 2
# 2 == vec$getmean() # TRUE
# newRawVec = c(3, 3, 3)
# vec$set(newRawVec)
# identical(newRawVec, vec$get()) # TRUE
# is.null(vec$getmean()) # TRUE - cached mean invalidated
# cachemean(vec) # 3
# 3 == vec$getmean() # TRUE
