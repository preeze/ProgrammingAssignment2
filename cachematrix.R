## Creates a mutable holder for matrix values together with its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(data) {
        x <<- data
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(val) inv <<- val
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Obtains an inverse for matrix x.
## If the inverse has already been calculated, simply retrieves it from the cache;
## otherwise calculates the value and caches it for future.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (is.null(inv)) {
        message('Calculating inverse')
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
    } else {
        message('Getting cached inverse')
    }
    inv
}

# check inverse calculation and caching logic
src <- matrix(c(1, 2, 3, 4), 2, 2)
cacheSrc <- makeCacheMatrix(src)
# when executed, the first message shall say 'Calculating inverse' and
# all the subsequent ones must yield 'Getting cached inverse'.
# Inverse matrix will be printed out each time and they all shall be equal.
cacheSolve(cacheSrc)
cacheSolve(cacheSrc)
cacheSolve(cacheSrc)
