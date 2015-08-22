## Put comments here that give an overall description of what your
## functions do

## My functions compute the inverse of the matrix with caching.
## First of the functions generates a structure 
## which encapsulates the matrix and it's inverse value.
## The other function uses that special object to store
## the computed inverse in it along with the corresponding
## matrix.

## This function wraps a matrix into a structure 
## and returns that structure.
## The structure has four functions in it:
## get - returns the wrapped matrix
## set - updates the wrapped matrix and resets cache of inverse to NULL
## getinv - returns cached inverse of a matrix 
##          can return NULL if there is no calculated value
## setinv - updates cached inverse with the value given

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function uses cache matrix structure to cache matrix
## inverse calculation.
## If there is already a computed inverse it just returns its value
## otherwise it computes the inverse, updates its value inside
## chache matrix structure, and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
