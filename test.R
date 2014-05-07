## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##calculate teh inverse of matrix
  getinverse <-function() m<<-solve(x) 
  list(getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ##return the inverse matrix
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  
}