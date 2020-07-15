## Week 3 Assignment, matrix inversion. Comprises two functions in order
## to complete matrix inversion

## This makeCacheMatrix function creates a matrix object that can cache its 
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <-function() x
        setinverse <-function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Thus cacheSolve function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If this has been calculated previously and the
## matrix has not changed, the inverse matrix will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("Please be patient, getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}