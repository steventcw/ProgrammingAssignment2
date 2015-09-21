## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. The functions below would cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
    getInverseMatrix <- function() im
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	im <- x$getInverseMatrix()
    if(!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    m <- x$get()
    im <- solve(m, ...)
    x$setInverseMatrix(im)
    im
}
