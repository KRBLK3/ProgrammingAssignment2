makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

cacheSolve <- function(x) {
    matrix <- x$getInverse()
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data)
    x$setInverse(matrix, ...)
    matrix
}


