## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
