## these two function combined will allow you to take a matrix and
## save the inverse value of the matrix
## you first need to assign a new data variable with the makeCacheMatrix funciton
## you can then pass this variable through the cacheSolve function
## if it is the first time through the cacheSolve function it will calcuate the
## inverse, if it any time past that the cacheSolve will return the cached value
## and will not need to recalculate the data

## makeCacheMatrix will first test to see if you are entering a matrix into 
## the function, it will then check to make sure the function is square.
## if either of those are false if will give you a message to stop.
## next it will make a list of 4 different situations which will be activated
## by the next function
## this function will save your data if you assign it to a data variable

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


## This function will take the variable that you have assigned from the
## makeCacheMatrix and if you have not already solved the inverse it will call
## the get value from the variable (which is the original matrix) and 
## it will then solve the inverse of the matrix
## it then will modify the set_inverse value in the variable
## it then prints the solved inverse matrix
## if the inverse has already been solved using these two functions the casheSolve
## function will print the get_inverse value which is the solved inverse of the
## original matrix

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
