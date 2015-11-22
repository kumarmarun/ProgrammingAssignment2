## Create a special matrix object (with a matrix input x) that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
                ix <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(invx) ix <<- invx
                getinv <- function() ix
                list(set = set, get = get, 
                        setinv = setinv, getinv = getinv)

}


## Returns the inverse of the matrix object created by makeCacheMatrix. If
## the inverse was previously computed already, the cached inverse is
## returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                ix <- x$getinv()
                if (!is.null(ix)) {
                        message("getting cached inverse")
                        return(ix)
                }
                datamat <- x$get()
                ix <- solve(datamat, ...)
                x$setinv(ix)
                ix
                
}
