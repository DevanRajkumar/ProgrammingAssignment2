## Matrix inversion is usually a costly computation. Sometimes, it is a benefit
## to cache the inverse of a matrix rather than compute it repeatedly. These
## two functions are used to create a special object that stores a matrix and
## cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse.
    m <- NULL
    
    ## Set value of matrix. Initialize inverse.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Return matrix.
    get <- function() x
    
    ## Set the value of the inverse.
    setinverse <- function(inverse) m <<- inverse
    
    ## Return the value of the inverse.
    getinverse <- function() m
    
    ## Create special matrix object/list to store matrix and cache inverse.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes inverse of special "matrix" returned by makeCacheMatrix. If inverse
## has already been calculated (and matrix has not changed), retrieves inverse 
## from the cache. Otherwise, solves inverse of matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    ## Return inverse if cached.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, solve inverse of matrix, cache, and return inverse.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
