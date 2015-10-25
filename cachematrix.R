## makeCacheMatrix This function implements a special "matrix" object that cache its inverse.
## cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

## makeCacheMatrix stores matrices in memory using scoping rules

makeCacheMatrix <- function(X = matrix()) {
 inv <- NULL
 set <- function(Y) {
   X <<- Y
   inv <<- NULL
 }

 get <- function() X
 setinv <- function(Inv) inv <<- Inv
 getinv <- function() inv
 list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve uses the Pseudoinverse function of the corpcor package, which implements Moore–Penrose pseudoinverse, to calculate the inverse matrix of X

cacheSolve <- function(X, ...) {
 if (!require("corpcor")) {
  install.packages("corpcor")
  if (!require(corpcor)) {
   stop("corpcor package is required to execute this function")
  }
 }
 inv <- X$getinv()
 if (!is.null(inv)) {
  message("matrix is in memory")
  return(inv)
 }

 message("inverse is missing, the inverse will be computed")
 data <- X$get()
 inv <- pseudoinverse(data, ...)
 X$setinv(inv)
 message("inverse is the following: ")
 inv
}
