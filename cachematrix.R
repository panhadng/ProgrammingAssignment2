#RATANAK PANHA DUONG
#LEXICAL SCOPING ASSIGNMENT

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special matrix, containing the functions to set values and get values of the matrix as well as set values and get values of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {v <<- inverse}
    getinverse <- function() {inverse}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the "matrix" created by the first function. It will check to see if the inverse of the matrix has already been calculated.
# If so, it will retrieve the inverse from the cache and skip the computation. Otherwise, it will calculate the inverse and produce it by itself via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getInverse
    if(!is.null(v)){
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v
}
