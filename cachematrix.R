## This program will create a matrix and cache the inverse to allow for later
## without the need to re-compute the matrix each time.

## First function creates our matrix, gets the inverse, and caches the inverse.
library(MASS)
makeCacheMatrix <- function (x = numeric()){
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get<- function()x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function(){
                inver <- ginv(x)
                inver%*%x   ##obtains the inverse of the matrix
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function (x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        m=inv ## Returns a matrix being the inverse of 'x'
}