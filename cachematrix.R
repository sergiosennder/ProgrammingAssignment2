## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## Given a matrix object, return a cache representation of the 
## matrix, which can be used to store (i.e. table) calculations 
## such as the inverse matrix, consequently increasing
## performance by calculating once and reading many times.
##
## args:
## 'x' is a matrix or an object converted into a matrix
##
## output:
## a list representing a 'cached-matrix' 

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse matrix place holder
    inv <<- NULL
    
    # associate a new matrix with the 'cached-matrix'
    set <- function(x = matrix()) {
        x <<- x
        inv <<- NULL
    }
    
    # return the contained matrix
    get <- function() {
        x
    }
    
    # set a calculated inverse to the contained matrix
    setInverse <- function(y) {
        inv <<- y
    }
    
    # get a stored previously calulated inverse to the 
    # contained matrix
    getInverse <- function() {
        inv
    }
    
    # return a list representation of the 'cached-matrix'
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve
## Return a stored or calculated inverse matrix for a 
## 'cached-matrix' created via makeCacheMatrix.
## 
## args:
## 'x' is a matrix created with makeCacheMatrix
## '...' are additional arguments to pass to the 'solve' function
##  used internally to calculate the inverse matrix.
##
## output:
## function returns either the previously stored inverse matrix,
## or a newly calculated (then stored) inverse matrix

cacheSolve <- function(x, ...) {
    
    # return a stored previously calcualted inverse if one exists
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return (inverse)
    }
    
    # otherwise, calculate, store, then return the inverse
    matrix <- x$get()
    inverse <- solve(x$get())
    x$setInverse(inverse)
    
    return (inverse)
}
