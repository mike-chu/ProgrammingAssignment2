## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is a list of functions:
## 1. set() - set the values of the matrix
## 2. get() - get the values of the matrix
## 3. setinverse() - set ("cache") the value of its matrix inverse
## 4. getinverse() - get the value of its matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## When changing the value of matrix, the "cached" inv needs to be cleared
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    ## return as list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix()
## It first attempts to retrieve the "cached" inverse and avoid re-computation of solve();
## If not successful, this function will use solve() to calculate the matrix inverse as well as
## saving the inverse into the cache of the special "matrix"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## First to attempt to retrieve the "cached" inverse
    if (!is.null(inv)) {
        message("getting cachced inverse")
        return (inv)
    }
    data <- x$get()
    ## solve() is function to compute the matrix inverse
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
