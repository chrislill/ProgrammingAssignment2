## Function object to act as a local cache for a matrix and it's inverse.
##
## Arguments:
##     x: The matrix to be inverted.
##
## Returns:
##     List of functions to access the cache from other environments:
##         set, get, setinverse, getinverse.
##
makeCacheMatrix <- function(x = matrix()) {
    
    # Instantiate the inverse (i) as null.
    i <- NULL
    
    # Set the cache for the original matrix, resetting the inverse (i) to null.
    # Use the <<- operator to modify the x and i objects in the 
    # makeCacheMatrix (parent) environment, from the context of the 
    # get and set (child) environments.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Get the original matrix from the makeCacheMatrix (parent) environment
    # using lexical scoping. 
    get <- function() x
    
    # Repeat for caching the inverse of the matrix.
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # Expose a list of get and set functions so they can be called from other 
    # environments.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Retrieves the inverse of a matrix from a cache, calculating the inverse
## using the solve function if necessary.
## 
## Arguments:
##     x:   makeCacheMatrix object to store the matrix and cache it's inverse.
##     ...: Other arguments to pass to the solve function.
##
## Returns:
##     The inverse of the matrix.
##
## Notes:
##     1. Displays a message if the inverse has been retrieved from the cache.
##     2. Assumes that the submitted matrix is always invertible.
##
cacheSolve <- function(x, ...) {
    
    # Try retrieving the inverse of the matrix from the cache.
    i <- x$getinverse()
    
    # If the cached value is not null, return it.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Retrieve the original matrix from the cache.
    data <- x$get()
    
    # Find the inverse using solve.
    i <- solve(data, ...)
    
    # Cache the inverse then return it.
    x$setinverse(i)
    i

}
