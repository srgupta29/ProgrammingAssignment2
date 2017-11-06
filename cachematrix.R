## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix and returns a list of function 
##that can be applied on that matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- matrix(NA)
      set <- function (y){
        x <<- y 
        inv <<- matrix(NA)
      }
      get <- function() x
      setinv <- function(inv_mat) inv <<- inv_mat
      getinv <- function() inv

      list(set = set , get = get , setinv = setinv , getinv = getinv)
}


## this function operates on special matrix created by "makeCacheMatrix" , 
## and returns the cached inverse of the matrix if available 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(all(!is.na(inv))){
          return (inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
        inv
}
