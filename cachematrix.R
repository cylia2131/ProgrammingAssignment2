## Caching the Inverse of a Matrix
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
## Initialize the inverse property
        
        i <- NULL
        
## Method to set the matrix
        
        set <- function(y) {
                x <<- y
                i <<- NULL
  }
## Method the get the matrix      
        get <- function() x
        
## Method to set the inverse of the matrix
        
        setinverse <- function(inverse) i <<- inverse
        
## Method to get the inverse of the matrix
        
        getinverse <- function() {
 ## Return the inverse property
                i
        }
## Return a list of the methods        
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed), 
##then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
## Return the inverse if its already set        
        if (!is.null(i)) {
                 message("getting cached data")
                 return(i)
        }
## Get the matrix 
        data <- x$get()
        
## Calculate the inverse using matrix multiplication
        
        i <- solve(data, ...)
        x$setinverse(i)
## Return the matrix
        i

}

