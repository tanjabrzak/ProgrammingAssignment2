## Solution for Programming Assignment 2 in a R Programming course
## Coursera, Johns Hopkins University
## Author: Tatjana Brzak
## 2017-30-11

## Used Google's R Style Guide
## https://google.github.io/styleguide/Rguide.xml

makeCacheMatrix <- function(p.matrix = numeric()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   p.matrix: Input matrix, must be square (2x2, 3x3 etc.).
    #
    # Returns:
    #   List of functions: (set, get, setInverse, getInverse).
    inverse <- NULL
    
    # Set the value of the matrix
    set <- function(y) {
        p.matrix <<- y
        inverse <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() {
        p.matrix
    }
    
    # Set the inverse matrix
    setInverse <- function(solve) {
        inverse <<- solve
    }
    
    # Get the inverse matrix
    getInverse <- function() {
        inverse
    }
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

cacheSolve <- function(p.cmatrix, ...) {
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix.
    # If the inverse has already been calculated (and the matrix has not changed),
    # then the cacheSolve should retrieve the inverse from the cache.
    #
    # Args:
    #   p.cmatrix: Input list returned by makeCacheMatrix above.
    #
    # Returns:
    #   Inversed matrix.
    
    # Get the inverse matrix from the cache
    inverse <- p.cmatrix$getInverse()
    
    # Check if matrix is already cached
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If matrix is not cached proceed with caching
    data <- p.cmatrix$get()
    
    inverse <- solve(data, ...)
    
    # Put the inverse matrix to the cache
    p.cmatrix$setInverse(inverse)
    
    inverse
}

# Example:
#
# my_matrix <- matrix(1:4, nrow = 2, ncol = 2)
#
# my_cache <- makeCacheMatrix(my_matrix)
#
# cacheSolve(my_cache)  ## first time not returned from the cache
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
# cacheSolve(my_cache)  ## second time returned from the cache
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
