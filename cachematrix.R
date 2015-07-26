## A set of functions for calculating and caching the inverse of a matrix

## Creates an object that allows for the inverse of a matrix to be set and
## retrieved for the purpose of caching
## Args:
##   x: A matrix
## 
## Returns:
##   A list of four functions (set, get, setinverse, getinverse) which allow
##   the setting and retrieval of a matrix and the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # Set the inverse to null so we can check if it has been set or not
    inv <- NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse of the matrix 
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse of the matrix; it will be null if it hasn't been set yet
    getinverse <- function() inv
    
    # Return a list with all four functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix associated with the object created by the 
## makeCacheMatrix function. If the inverse of the matrix has been cached then
## it will not be recalculated
##
## Args:
##   x: An object created by the makeCacheMatrix function
##   ...: Arguments passed to the solve function when calculating the inverse
##
## Returns:
##   The inverse of the matrix associated with x
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # If the inverse isn't null then it has been calculated and cached so it can
    # be returned straight away
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix
    data <- x$get()
    
    # Calculate the inverse of the matrix
    inv <- solve(data, ...)
    
    # Set/cache the calculated inverse for future reference
    x$setinverse(inv)
    
    # Return the inverse
    inv
}
