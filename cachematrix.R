## Finds inverse of a matrix and stores it.
##Checks if inverse has been found. If not, calculates it. 


## Creates the empty matrix that will be used
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
## sets initial values
        setInverse <- function(inv) m <<- inv
        getInverse <- function () m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        }
##Checks if inverse matrix has been calculated. If so, returns inverse. 
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
##If not, calculates inverse. 
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
