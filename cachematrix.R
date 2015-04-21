# Pair of functions that cache the inverse of a matrix; run makeCacheMatrix; set data; run cacheSolve

# Creates a special matrix containing list of functions to set matrix values, get matrix values, set matrix inverse and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Argument is the special matrix created by makeCacheMatrix; gets inverse from cache if previously computed; calculates inverse of matrix if not previously computed.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
