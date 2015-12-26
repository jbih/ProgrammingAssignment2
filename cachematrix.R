###################################################################################
# The makeCacheMatrix creates a special "matrix" object that can cache its inverse
# This functioncreates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
###################################################################################
makeCacheMatrix <- function(x = matrix()) 
{
    in_ <- NULL
    set <- function(y) {
        x <<- y
        in_ <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) in_ <<- inverse
    getinverse <- function() in_
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

###################################################################################
# The cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
###################################################################################
cacheSolve <- function(x, ...) {
    in_ <- x$getinverse()
    if(!is.null(in_)) # retrieve previous calculated 
    {
        message("Getting cached data.")
        return(in_)
    }
    data <- x$get()
    in_ <- solve(data)
    x$setinverse(in_)
    in_
}
