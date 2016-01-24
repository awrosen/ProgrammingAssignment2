## makeCacheMatrix creates a matrix for cacheing it's inverse
## cacheSolve computes or returns the inverse matrix form makeChacheMatrix

## Write a short comment describing this function:
## The function set and gets the value of the matrix.
##Afterwards the function set and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
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


## Write a short comment describing this function
## The function returns the inverse of the matrix.
## It first checks if it has already been calculated, if so, then it get and prints the results.
## Else the function calculated the inverse matrix and put the value in catch via setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {		
                message("Getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv 
}
