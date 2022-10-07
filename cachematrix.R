## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


library(MASS)
makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL         ##inverse is NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x       ##funtion used to get matrix x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() {
    inver <-ginv(x)
    inver%*%x        ##this function used to get inverse of the matrix
  }
  list(set = set, get= get,
       setinv = setinv, 
       getinv = getinv)
}

cacheSolve <- function(x, ...) ##obtains cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

f <- makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)