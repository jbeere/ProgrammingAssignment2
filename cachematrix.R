## these functions can be used to calculate and cache the
## inverse of a matrix
## example usage:
##   m <- makeCacheMatrix(my_matrix)
##   cacheSolve(m)  ## will calculated value and cache
##   cacheSolve(m)  ## will return cached value

## creates an object that will hold a matrix and it's inverse
## the cached inverse value can be calculated using cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## returns the inverse of the matrix in x
## caches the inverse into x
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- x$get()
    message("calculating and caching inverse")
    i <- solve(data) %*% data
    x$setinverse(i)
    i
}
