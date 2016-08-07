# Author: mdpmanchester 
#
# Functions to create a cached matrix
#

makeCacheMatrix <- function(x = matrix()) {
    # Get a special "matrix" object that can cache its inverse.
    # Args:
    #   x: matrix
    # Returns:
    #   list containing functions to set and get the values of the matrix and it's inverse
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse_matrix) inverse <<- inverse_matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Get the matrix that is the inverse of 'x'

    # Args: 
    #   x: matrix object to calculate inverse on
    #
    # Returns: 
    #   The inverse of matrix x
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    the_matrix <- x$get()
    inverse <- solve(the_matrix)
    x$setinverse(inverse)
    inverse

}
