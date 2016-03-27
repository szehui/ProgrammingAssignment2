## MakeCacheMatrix stores the inverted matrix that cacheSolve checks against.
## If the inverted matrix is not available, cacheSolve will then solve the matrix
## and return its inverse.

## MakeCacheMatrix will be an object that stores the inverted matrix that CacheSolve
##checks against

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(inverted) m <<- inverted
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## cacheSolve first checks for the presence of the matrix, and returns the inverse.
## If not found, it will run the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
        
}
