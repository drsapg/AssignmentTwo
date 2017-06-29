## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function:
## Set the matrix and erase the matrix cache if exists
## Set the inverse of matrix
## Get the inverse
## Return a list

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(invertir) inverse <- sort(x)
        get_inverse <- function() inverse
        
        list(set = set, get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## This function verify if a inverse exist on cache and return it else if calculate them
## Get the inverse in cache
## if found it return a message and the inverse
## else calculate the inverse

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if (!is.null(inverse)) {
                message("Already calculated")
                return(inverse)
        }
        mymatrix <- x$get()
        inverse <- solve(mymatrix)
        x$set_inverse(inverse)
        inverse
}
