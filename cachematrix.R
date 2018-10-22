## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned bby makeCacheMatrix.
## If the inverse has already been calculated. then cacheSolve should retrieve the inverse 
## from the chache.

cacheSolve <- function(x, ...) {
       inv <- x$getInv()
       if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
       }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInv(inv)
       inv
}