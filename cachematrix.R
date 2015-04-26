## This file contains two functions.
## The first function, makeCacheMatrix, sets and gets a matrix and then sets and gets the inverse of the matrix
## The second function, cacheSolve, calculates and returns the inverse of the matrix defined with the first function

makeCacheMatrix <- function(x = matrix()) {
        ## Set the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(xInverse) m <<- xInverse
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## In the next function, it first checks if the inverse of the matrix is available.
## If available, it returns the inverse of the matrix in the cache via the getinv function

cacheSolve <- function(x, ...) {
        ## Check if the inverse of the matrix is available.
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If inverse is not available, calculate the inverse of the matrix
        ## Then set the value of the inverse of the matrix in the cache via the setinv function.
        data <-x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
