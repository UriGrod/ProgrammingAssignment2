## 

## The function below is a getter setter - defining both the object (matrix and
##it's inverse), and the methods to get those or set those.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}


## This function will return the inversed matrix if already exists. 
## Otherwise, it will inverse (solve) it, used the object created by the above
##function to set the inversed matrix as the cached value, and return the inverse. 

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("the inverse of this matrix has already been cached. Returning cached inverted-matrix:")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
        
        }
        
        
        ## Return a matrix that is the inverse of 'x'
