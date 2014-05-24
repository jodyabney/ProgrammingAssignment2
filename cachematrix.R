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
makeCacheMatrix <- function(x = matrix()) {
    
    # start with a NULL inverted matrix
    im <- NULL
    
    # store a given square matrix in the "special" matrix
    set <- function(y) {
        # make sure we are trying to store a matrix
        if(!is.matrix(y)) {
            message("not a matrix")
            return()
        }
        
        # make sure we are trying to store a square matrix
        y_dims = dim(y)
        if(!(dim[1] == dim[2])) {
            message("not a square matrix")
            return()
        }
        
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverted()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    data <- x$get()
    im <- solve(data)
    x$setinverted(im)
    im
}
