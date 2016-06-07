## Cache the inverse of a matrix:
## Inverting a matrix is time-consuming, so create a matrix object that stores its own inverse

## makeCacheMatrix gets (reads) and sets (writes) the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## The inside functions' parent environment (an instance of makeCacheMatrix) will be saved and mutable,
    ## so a matrix created (set) by makeCacheMatrix() will have both x and inv variables saved,
    ## even after these functions finish executing
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(mean) inv <<- mean
    getInv <- function() inv
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## cacheSolve checks if the inverse exists and solves it if it doesn't exist

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}


## Let's see if it works using a 2x2 matrix A and its inverse B
A <- matrix(c(1,2,3,4), 2, 2)
A
B <- solve(A)
B
## A matrix multiplied by its inverse should generate an identity matrix
A %*% B

## Now let's do this with our caching functions
C <- makeCacheMatrix( matrix(c(1,2,3,4), 2, 2) )
C$get()
C$getInv()
cacheSolve(C)
C$getInv()
C$get() %*% C$getInv()
## This produced the same results as A and B