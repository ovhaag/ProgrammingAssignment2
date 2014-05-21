## programming assignment for coursera course on R programming
## Set of R functions to cache potentially time-consuming matrix inversion.

## creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(original = matrix()) {
    inverse <- NULL
    set <- function(original_matrix) {
        original <<- original_matrix
        inverse <<- NULL
    }
    get <- function() original
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         setinv = set_inverse,
         getinv = get_inverse)
}


## computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix`. 
## If the inverse has already been calculated 
## and the matrix has not changed, then
## `cacheSolve` retrieves the inverse from the cache.
cacheSolve <- function(cm, ...) {
    inv <- cm$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- cm$get()
    inv <- solve(data, ...)
    cm$setinv(inv)
    inv
}
