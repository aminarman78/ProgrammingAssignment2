
# makeCacheMatrix creates a matrix that can cache the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    #Initiallizing
    inv <- NULL
    # setting our matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #getting our matrix
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    # A list which sotres all we'd defined above
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#Compute the inverse of the special matrix returned by "makeCacheMatrix". if makeCacheMatrix wasn't empty, it will compute and return matrix inverse
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
