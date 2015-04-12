## Function object to act as a local cache for a matrix and it's inverse
##
## Arguments:
##     x: The matrix to be inverted
##
## Returns:
##     List of functions to access the cache from other environments:
##         set, get, setinverse, getinverse
##
makeCacheMatrix <- function(x = matrix()) {
    
    # Set the inverse (i) to null when creating the object
    i <- NULL
    
    # Use the <<- operator to modify the x and i objects in the 
    # makeCacheMatrix (parent) environment, from the context of the 
    # get and set (child) environments
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # Expose a list of get and set functions so they can be called from other 
    # environments
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Retrieves the inverse of a matrix from a cache, calculating the inverse
## using solve() if necessary
## 
## Arguments:
##     x:   makeCacheMatrix object to store the matrix and cache it's inverse
##     ...: Other arguments to pass to the solve function
##
## Returns:
##     The inverse of the matrix
##
## Notes:
##     1. Displays a message if the inverse has been retrieved from the cache
##     2. Assumes that the submitted matrix is always invertible
##
cacheSolve <- function(x, ...) {
    
    # Retrieve the inverse of the matrix from the cache and return it if it
    # is not null
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Retrieve the original matrix from the cache, then solve it, cache it
    # and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i

}
