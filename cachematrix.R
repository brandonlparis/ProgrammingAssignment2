## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing functions to . . . 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## The second function, cacheSolve, calculates the inverse of the special
## "matrix" created with makeCacheMatrix.  However, it first checks to see
## if the inverse matrix has already been calculated. If so, it gets the
## inverse matrix from the cache and skips the computation. Otherwise, it
## calculates the inverse matrix of the data and sets the value of the
## inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Test Syntax
# a.mat <- matrix(rnorm(9), nrow=3)
# a.matlst <- makeCacheMatrix(matrix(rnorm(9), nrow=3))
# a.inv <- cacheSolve(a.matlst)
# verify <- round(a.matlst$get() %*% a.inv)