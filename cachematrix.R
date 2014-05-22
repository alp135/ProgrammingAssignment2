
## This function creates a matrix and is a list containing 4 functions:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Makes sure m is equal to NULL if there is no cached inverse
        # Function to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Function to get the value of the matrix
        get <- function() x
        
        # Function to set the value of the inverse
        setinverse <- function(inverse) m <<- inverse
        
        # Function to get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix. It first checks to see 
## if this has already been done. If so, it gets the inverse from the cache and
## skips the computation. If not, it will calculate the inverse of the matrix
## sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {				# If m is NOT null it means there is a cached inverse
                message("getting cached data")  # So then it gets the cached data
                return(m) 				# And just returns this data: the inverse
        }
        data <- x$get()					# If m is null, we have to get the value of the matrix 	
        m <- solve(data, ...) 			# Calculate the inverse of this matrix
        x$setinverse(m)					# And cache it
        m
}     

