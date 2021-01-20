## These functions are used to cache the inverse of a matrix


## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(1:4,2,2)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, the inverse from the cache is 
## retrieved.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting matrix inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
        ## Return a matrix that is the inverse of 'x'
