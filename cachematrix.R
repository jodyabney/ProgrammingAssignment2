######################################################################
## R Programming Course: Programming Assignment #2                  ##
## Coursera: John Hopkins Bloomberg School of Public Health         ##
## Coursera Specialization: John Hopkins Data Science               ##
## Course: R Programming                                            ##
## Author: Jody P. Abney                                            ##
######################################################################

## makeCacheMatrix(x = matrix) and cacheSolve(x,...) are a set of functions
## to used on a given square (invertible) matrices for the following purposes:
## 1) Conceptual
##    a) Demonstrate the lexical scoping environment concepts in R
##    b) Reinforce the idea that everything in R is an object which
##       can be used/messaged between objects
## 2) Practical
##    a) Since matrix inversion is an expensive process in R, these functions
##       cooperate to cache the inverse of the given square matrix so it's only
##       calculated once until the given matrix is changed which leads to a
##       new inverted matrix when needed

## makeCacheMatrix(x = matrix) takes a square matrix and converts it
## to a "special" matrix that has "messages" in a list that can be applied
## to it:
##    1) $set - takes a square matrix and stores it in "special" matrix
##    2) $get - returns the square matrix stored in the "special" matrix
##    3) $setinverted - stores the inverted matrix in the "special" matrix
##    4) $getinverted - returns the inverted matrix stored in the "special"
##       matrix
## The function also includes checks to ensure we are trying to store 
## a square matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # start with a NULL inverted matrix
    im <- NULL
    
    # store a given square matrix in the "special" matrix
    set <- function(y) {        
        x <<- y         ## store the square matrix      
        im <<- NULL     ## reset the inverted matrix to NULL
    }
    
    # return the stored square matrix
    get <- function() x
    
    # store the inverted matrix
    setinverted <- function(solve) im <<- solve
    
    # return the stored inverted matrix
    getinverted <- function() im
    
    # set up the object "messages"
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## cacheSolve(x,...) takes a makeCacheMatrix "special" matrix as input and
## returns the inverted matrix based on the following:
##   1) If we already have a cached inverted matrix, then return it; else
##   2) Calculate the inverted matrix and cache it for later

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverted() ## get the stored inverted matrix from the "special" matrix
    if(!is.null(im)) { ## if we have a cached inverted matrix, return it
        message("getting cached data")
        return(im)
    }
    
    ## Calculate the inverted matrix if needed
    data <- x$get()   # get the square matrix
    im <- solve(data) # calculate inverted matrix, a relatively expensive operation
    x$setinverted(im) # cache the inverted matrix
    im                # return the inverted matrix
}
