##
# Data Science - Bloc 2: R-Programming
# Week 3 - Assignment 2
# cachematrix.R
# Code that cahce the inverse of an invertible matrix
# Include 2 functions: makeCacheMatrix and cacheSolve
##


## Function that defined 4 subfunctions to handle a cache.
# input: matrix to inverse (assuming the matrix supplied is invertible)
# output: special matrix information in a list of 4 functions
makeCacheMatrix <- function(m = matrix()) {
    # initialized the inverse_matrix object
    inverse_matrix <- NULL
    
    # function to set matrix value & reinitialize the cache
    set <- function(y) {
        m <<- y
        inverse_matrix <<- NULL
    }
    # function to get matrix value
    get <- function() m
    # function to set/solve matrix inverse
    setinverse <- function(solve) inverse_matrix <<- solve
    # function to get inverse_matrix from the cache
    getinverse <- function() inverse_matrix
    
    #return a list with the function defined in makeCacheMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## Check in the cache if the inverse of a matrix result already existe 
# and return it's value. If not, it computes the inverse and save the 
# result in the cache.
# input : output of makeCacheMatrix function
# ouput: matrix inverse of the one use as input argument in makeCacheMatrix
cacheSolve <- function(m) {
    
    # fetch cache value
    inverse_matrix <- m$getinverse()
    
    if (!is.null(inverse_matrix)){ # if cache not empty, return it
        message("getting cached matrix inverse")
        return(inverse_matrix)
    }
    
    # if cache empty, compute the inverse matrix and return it
    data <- m$get()   # get value of matrix
    inverse_matrix <- solve(data)  # compute the inverse matrix
    m$setinverse(inverse_matrix)   # put the result in the cache for futur computation
    
    # return
    inverse_matrix
}

