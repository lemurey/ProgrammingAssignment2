## Two functions which allow for storing the inverse of a matrix so it does not 
## have to be recalculated


## This function takes a matrix as input and creates 4 sub-functions used to 
## store the inverse later. get, set, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) {
     #initializes the inverse value to NULL
     inv <- NULL
     
     set <- function(y) {
          ## this sets the matrix to a new value, so variable name stays the same
          ## but the matrix values can be reset by the user, also resets the inverse
          ## uses <<- operator rather than <- operator which causes R to search for
          ## a previously defined value and replaces it, rather than creating a new
          ## value in the function enviroment
          x <<- y
          inv <<- NULL
     }
     
     get <- function() {
          ## returns the value of the matrix
          x
     }
     
     setinv <- function(inverse) {
          ## sets the value of the inverse using <<- for proper scoping
          inv <<- inverse
     }
     
     getinv <- function() {
          ## returns the value of the inverse
          inv
     }
     
     ## returns the functions defined above
     list(set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function solves a matrix stored in a list output by makeCacheMatrix
## if the inverse has already been calculated it grabs that from the cache
## rather than re-solving

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## first check if there is already a stored value of the inverse
     inv <- x$getinv()
     if (!is.null(inv)) {
          ## if there is a stored value, return it
          message('getting cached inverse')
          return(inv)
     }
     
     ## if there isn't a stored inverse, grab the matrix for inverting
     values <- x$get()
     ## check to make sure it's actually a matrix
     if (!is.matrix(values)) {
          message('you can\'t invert a non-matrix')
          return()
     }
     
     ## get the inverse and return it
     inv <- solve(values)
     x$setinv(inv)
     inv
}
