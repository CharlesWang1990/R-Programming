## Write two functions to return cached value for inverse of a matrix, if calculated already, or
##calculate the inverse if the inverse was not calculated.

## Write a function that takes a matrix as an input and returns a list of 4 functions to read the value of the input matrix, store the inverse of
## the matrix, set a new value to the input matrix, and read the value of the stored inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  readinput <- function () x
  setnewinput <- function(y){x <<-y ; inv <<- NULL }
  storeinverse <- function(inverse) inv <<- inverse  
  readinverse <- function() inv
  list(readinput=readinput,setnewinput=setnewinput,storeinverse=storeinverse,readinverse=readinverse)
  

}


## Read the cached inverse matrix, if caclulated already, or else calculate the inverse from scratch

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$readinverse()
  if(!is.null(inv)){message("getting cached data");return(inv)}
  newinv <- solve(x$readinput())
  x$storeinverse(newinv)
  newinv
  
  
}
