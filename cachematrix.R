## Make a list object of a matrix and cache computed inverse matrix


## Make a list of methods for setting and getting matrix and the corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   inverted <- NULL
   setmat <- function(y){
      x <<- y
      inverted <<- NULL
   }
   
   getmat <- function() x
   setinv <- function (inv) inverted <<- inv
   getinv <- function() inverted
   list (setmat = setmat, getmat = getmat,
         setinv = setinv, getinv = getinv)
}


## Reurn inverse matrix if cached or compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
           message("getting cached data")
           return(inv)
        }
        mat <- x$getmat()
        inv <- solve(x,...) %*% x
        x$setinv(inv)
        inv
         
}
