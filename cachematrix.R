## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        # Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Get the value of the matrix
        get <- function() x
        
        # Set the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        
        # Get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}        ## Return a matrix that is the inverse of 'x'

