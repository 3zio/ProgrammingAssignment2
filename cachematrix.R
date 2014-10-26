## Put comments here that give an overall description of what your
## functions do

# These functions compute and cache the inverse of a matrix so that
# it can be reused without repeating the computation

## Write a short comment describing this function
# This function generates a list of functions that can cache the inverse of a matrix
# and can retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This function calculates the inverse of a matrix by using the above function
# If the inverse is already cached, its value is simply retrieved and the computation is skipped

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
