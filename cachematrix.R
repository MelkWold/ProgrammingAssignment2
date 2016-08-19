## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates an object (x, a matrix) and caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y){
                x <<- y
                inverse <<- NULL
        }
        get <- function () x
        set_inverse <- function (inverse) inverse <<- inverse
        get_inverse <- function()inverse
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## Write a short comment describing this function
## The cacheSolve function returns an inverse of the matrix cached by the previous funciton. 
## The function first searches the inverse in the cached data, and if it doesn't find it, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)){
                message("getting cached data.")
                return (inverse)
        }
        data <- x$inverse()
        inverse <- inverse(data, ...)
        x$set_inverse(inverse)
        inverse
        
}
