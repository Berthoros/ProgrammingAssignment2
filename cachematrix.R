## This function is a list of functions which
     ## sets the value of a matrix
     ## gets the value of a matrix
     ## sets the value of the inverse of a matrix
     ## gets the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     inv  <- NULL
     set  <- function(y){
          x <<- y
          inv <<- NULL 
     }
     get  <- function() x
     setinverse  <- function(inverse) inv  <<- inverse
     getinverse  <- function() inv
     list(set= set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## The following function calculates the inverse of a matrix 
## if the value of this inverse is not saved in the cache
## in order to reduce the use of resources.

cacheSolve <- function(x, ...) 
{     
     inv  <- x$getinverse()
     if (!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data  <- x$get()
     inv  <- solve(data, ...)
     x$setinverse(inv)
     inv
}

