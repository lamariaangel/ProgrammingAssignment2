## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix object of invertable matrix

makeCacheMatrix <- function(x=matrix){
    m <- NULL
    set <- function(y){
        x <<- y 
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function returns the inverse of a cached matrix

cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
