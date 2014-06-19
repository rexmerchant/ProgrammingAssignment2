## cachematrix.R
## 
## Rex Merchant
## June 18, 2014

## Starting with an n x n invertible matrix, calculate and store its inverse.  On subsequent
## calls, if the input matrix has not changed, and if the inverse has been stored, return the
## stored inverse.  Otherwise, calculate (and store) the new inverse.

## makeCacheMatrix:  Store the input matrix and define 4 functions Set, Get, Setinv and Getinv
##                   for storing and retrieving the input matrix and its inverse.

## Input:
##      y_mat:  n x n invertible square matrix defined and filled with data
##      before the functions are called.

## Output:
##      list of 4 functions

makeCacheMatrix <- function(x_mat = matrix()) {
        xinv_mat <- x_mat
        xinv_mat <- NULL
        set <- function(y_mat) {
                x_mat <<- y_mat
                xinv_mat <<- NULL
        }
        get <- function() x_mat
        setinv <- function(inverse) xinv_mat <<- inverse
        getinv <- function() xinv_mat

        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## cacheSolve:  Return the inverse of the matrix: stored copy if available; otherwise calculate
##              and store a new inverse for y_mat.

## Input:
##      x_mat:  Stored matrix, its inverse and functions to operate on it.

## Output:
##      Calculated or retrieved inverse of matrix y_mat defined in the calling environment.

cacheSolve <- function(x_mat) {
        xinv_mat <- x_mat$getinv()
        
        if(!is.null(xinv_mat) &&
                   dim(y_mat) == dim(x_mat$get()) &&
                   y_mat == x_mat$get()) {
                message("Getting cached data ...")
                return(xinv_mat)
        }
        
        x_mat$set(y_mat)
        xinv_mat <- solve(y_mat)
        x_mat$setinv(xinv_mat)
        xinv_mat
}


## Call should be similar to this:
##
##      set.seed(123)
##      y_mat <- matrix(rnorm(9, 5, 6), 3, 3)
##      mcm <- makeCacheMatrix(y_mat)
##      print(cacheSolve(mcm))