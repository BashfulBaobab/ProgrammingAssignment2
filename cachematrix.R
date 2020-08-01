## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() {
            x
        }
        setinv <- function(inv_val) {
            i <<- inv_val
        }
        getinv <- function() {
            i
        } 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
} 


## This function either solves to find the inverse of the matrix, as created by the function
## above, or retrieves it from cached memory if it has already been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("Accessing cache to retrieve data. Please wait for a few moments.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
