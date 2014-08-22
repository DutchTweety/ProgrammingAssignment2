## The following functions calculate the inverse of a matrix and
## stores the result in memory

## The makeCacheMatrix creates a special object that stores 
## a matrix and cache´s its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
   		x <<- y
   		inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get,
   		setinverse = setinverse,
   		getinverse = getinverse) 
}

## The following function checks if the inverse is already stored
## in memory and returns the result. If it is not stored, the function
## calcultates first the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting chached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
