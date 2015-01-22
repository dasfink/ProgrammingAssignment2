## Creates an special matrix object that supports caching the inverse
## of the matrix (cacheMatrix).  This object is created by calling 
## makeCacheMatrix(x = matrix()) and optionally passing in an initial matrix
## value.

## cacheSolve(x, ...) expects a cacheMatrix object and will return the inverse
## of the matrix, if the inverse was previously cached it the cached
## value will be returned.


## Create an cacheMatrix along with getter and setter functions that allow
## caching and setting the inverse.  The inverse, if set will be cleared
## if the matrix is changed.
## Postcondition: Special cacheMatrix object is returned, if a matrix value
##   is provided then the matrix will be set to this value
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the mean of a cacheMatrix object, created by called
## makeCacheMatrix which uses solve to calculate the inverse of the matrix.

## Precondition: X is a cacheMatrix with a valid value set and can be solved
## Postcondition: If the inverse has been cached the cached copy will be 
##   returned, otherwise the function will call solve and store the
##   result in the cacheMatrix object x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
