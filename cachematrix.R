## Creates matrices that are able to cache their own inverse
## and provides a function to calculate the inverse, using the cached inverse when it is valid

## Creates a 'special' matrix that can cache its inverse (can be extended to other operations)
## Creates the functions used for this special matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse 
    
    getInverse <- function() inv
    
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## This will find the invers of the special matrix, using the cached value when it is valid
## Will raise an error if the matrix is singular
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    theMatrix <- x$get()
    
    m <- solve(theMatrix, ...)
    x$setInverse(m)
    m
    
}
