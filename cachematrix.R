## "makeCacheMatrix" function caches the inverse of the matrix, whereas 
## "cacheSolve" function, first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from cache. Otherwise, it tries 
## to compute the inverse, and sets the value of this in the cache via 
## the setinverse function.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()) {
        
        inverse <- NULL
        
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function (inv) {
                inverse <<- inv
        }
        
        getinverse <- function() inverse
        
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix  
cacheSolve <- function(x, ...) {
        
        ## Try getting inverse from cache
        inverse <- x$getinverse()
        
        ## If it is not empty/null, return from cache
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## Otherwise, caclulate the inverse of the matrix, store it in cache
        ## and return it 
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}