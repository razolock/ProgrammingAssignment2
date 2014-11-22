## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    v <- NULL       
    get <- function() { x }     
    setinverse <- function(inverse)  { v <<- inverse }
    getinverse <- function() { v }    
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve function retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    v <- x$getinverse()
    if(!is.null(v)) {  
        message("getting cached data")  
        return(v)                       
    }
    data <- x$get()        
    v <- solve(data, ...)   
    x$setinverse(v)           
    v               
}