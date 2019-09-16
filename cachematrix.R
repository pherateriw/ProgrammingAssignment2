## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates an object with a matrix 
#   and a bunch of functions for using the matrix, 
#   caching the inverse, etc.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverted) x_inv <<- inverted
    get_inverse <- function() x_inv
    list(set = set, get = get, 
         set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
# This function takes a cache-capable matrix object
#   and creates the inverse if one does not already exist
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$get_inverse()
    if(!is.null(x_inv)){
        message("getting cached inverse")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$set_inverse(x_inv)
    x_inv
}
