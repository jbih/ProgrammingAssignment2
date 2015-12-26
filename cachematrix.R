## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##makeCacheMatrix <- function(x = matrix()) {
##}
## Write a short comment describing this function
#cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
##}

##############################################################
# The makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
##############################################################
makeCacheMatrix <- function(x = matrix()) {
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
#The cacheSolve function calculates the mean of the special "vector" created 
#with the above function. However, it first checks to see if the mean has already
#been calculated. If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean 
#in the cache via the setmean function.
###################################################################################
cacheSolve <- function(x, ...) {
    in_ <- x$getinverse()
    if(!is.null(in_)) {
        message("Getting cached data.")
        return(in_)
    }
    data <- x$get()
    in_ <- solve(data)
    x$setinverse(in_)
    in_
}
