## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     if (class(x)[1]!="matrix") {
          print("Stop: Input is not a matrix")
          return()
     }
     if (nrow(x)!=ncol(x)) {
          print("Stop: Matrix must be square")
          return()
     }
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     set_inverse <- function(solve) m <<- solve
     get_inverse <- function()m
     list(set = set, get = get, set_inverse = set_inverse,
          get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     m <- x$get_inverse()
     if(!is.null(m)) {
          print("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$set_inverse(m)
     m
}
