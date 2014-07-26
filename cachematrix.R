## Functions Caching the Inverse of a Matrix
## Code written for R 3.1.0 - Platform: x86_64-w64-mingw32/x64 (64-bit)

## makeCacheMatrix function creates a special "matrix"
## object that can cache its inverse.
##
## Args:
##   myMatrix: The square Matrix which has to be inverted.
##
## Returns:
##   a list containing
##      set: function to "store" the original matrix
##      get: function to "retrieve" the original matrix
##      setInverse: function to "store" the inverse matrix
##      getInverse: function to "retrieve" the inverse matrix

makeCacheMatrix <- function(myMatrix = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                myMatrix <<- y
                invMatrix <<- NULL
        }
        get <- function() myMatrix
        setInverse <- function(solve) invMatrix <<- solve
        getInverse <- function() invMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}


## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If already calculated & no change in matrix, retrieves it from cache.
##
## Args:
##   x: The special "Matrix" returned by makeCacheMatrix function
##      containing list of special functions (get, set, setInverse, getInverse) 
##
## Returns:
##   a matrix that is the inverse of original matrix

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix
}