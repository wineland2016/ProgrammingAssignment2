## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        if(!is.matrix(x)) stop("x must be a matrix")
  
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
  
        get <- function() x
        setInverse <- function(solve) invMatrix <<- solve(x)
        getInverse <- function() invMatrix
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix
}
