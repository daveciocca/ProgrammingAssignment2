## Put comments here that give an overall description of what your
## functions do

## The functions that follow, makeCacheMatrix and cacheSolve, are created to
## solve the computationally expensive task of matrix inversion more
## efficiently.


## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## The function will:
##      * Set the matrix values
##      * Get the matrix values
##      * Set the inverse of the matrix
##      * Get the invesre of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <-- y
                m <<- NULL
        }
        get <-function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get, setInv = setInv, getInv = getInv)

}

## Write a short comment describing this function

## cacheSolve computes the the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and 
## the matrix has not changed), cacheSolve should simply retrieve the inverse
## from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInv()
        if(!is.null(m)) {
                message("Getting cached matrix...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
