## These two functions work together to calculate the inverse
## of a matrix if it has not been calculated before and store 
## the result, otherwise return the inverse from a previous 
## calculation.  

## Function to create a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get= get, setinv = setinv, getinv = getinv)
}


## Function to retrieve the matrix from the 'makeCacheMatrix' function and 
## compute the matrix inverse if not already calculated, passing the result 
## back to the function as a cached result. If a cached result already exists,
## retrieve the cached result instead of performing the calculation.

cacheSolve <- function(x, ...) {
                i <-x$getinv()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinv(i)
                i
}
