## This is so far assignment fulfill
## which present two basic functions

## makeCacheMatrix will revert (inversing) matrix received as
## input and returning inversed matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
               x <<- y
               inv <<- NULL ## here's a special assignment operator used 

     }
     get <- function() x  ## resolving back a function
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Next function will calculate the inverse of the special 
## "matrix" returned by the previous function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
             message("Calculating cached input") ## Just some  basic message to be shown 
             return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(solve(inv))
        inv
}

m <- makeCacheMatrix(matrix(1:4, 2))
cacheSolve(m)

# This second call will use the cached matrix
cacheSolve(m)
