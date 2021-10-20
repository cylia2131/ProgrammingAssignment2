## Caching the Inverse of a Matrix
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse property
        
        i <- NULL
        
        ## set the matrix
        
        set <- function(y) {
                x <<- y
                i <<- NULL
  }
        get <- function() x
        
        ##set the inverse of the matrix
        
        setinverse <- function(inverse) i <<- inverse
        
        ##get the inverse of the matrix
        
        getinverse <- function() i
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if (!is.null(i)) {
                 message("getting cached data")
                 return(i)
        }
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        
        i <- solve(data, ...)
        x$setinverse(i)
           ## Return the matrix
        i

}

