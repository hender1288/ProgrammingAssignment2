## cachematrix.R aims to reuse the data store in cache in case that the 
## function is executed with the same arguments as the previous execution
## in order to save time and resources computing the matrix inverse

## makeCacheMatrix receive an inversable matrix as an argument
## and returns a list of 4 elements which are:
## [1] a function that sets the matrix to be inversed -->set
## [2] a function to get the matrix to be inversed --> get
## [3] a function for compute the inverse matrix -->setinv
## [4] a function for obtain the inverse matrix -->getinv


makeCacheMatrix <- function(mtrx = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    mtrx <<- y
    inv <<- NULL
  }
  get <- function() mtrx
  setinv <- function(inv) inv <<- solve(mtrx)
  getinv <- function() inv
  list(set = set, get = get,
       setinv= setinv,
       getinv = getinv)
  
}

## cachesolve will calculate the inverse recieving a list produced by makeCacheMatrix
cacheSolve  <- function(chmtrx, ...) {
  inv <- chmtrx$getinv()
  if(!is.null(inv)) {
    print("getting data fro cache")
    return(inv)
  }
  data <- chmtrx$get()
  inv <- solve(data, ...)
  chmtrx$setinv(inv)
  inv
}