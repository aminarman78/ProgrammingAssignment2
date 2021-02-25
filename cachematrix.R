makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



cacheSolve <- function(x) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}

#Example
x <- matrix(c(1,2,5,7),2,2)
y <- makeCacheMatrix(x)
xInverse <-  cacheSolve(y)
xInverse
