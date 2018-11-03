## The following two functions work toegether to cache the inverse of a matrix. 

## makeCacheMatrix creates and returns an R object that stores a matrix x (assume an 
## invertible matrix) and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve's argument is an object returned by makeCacheMatrix, then it retrieves
## the inverse either by retrieving it in the cache if it exists (including a message
## that the data was retrieved from storage) or calculating it if it has not yet been 
## stored. The inverse is then returned. 
cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    
    x$setinverse(i)
    i
}