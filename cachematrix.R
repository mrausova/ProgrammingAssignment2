
## The following functions together return an inverse of an invertible matrix x given as an input.
## The computation is done using caching so that the inversion is performed only when it has not been performed before.
## If it has been performed before, it just takes the cached value from previous computation.
## This is achieved by taking advantage of R's lexical scoping.


## The makeCacheMatrix function creates 4 functions that together are setting and getting
## the values of the input matrix x and of the inverse of corresponding matrix.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve function takes as input the result of makeCacheMatrix
## and returns the inverse of the matrix x that was used as input in makeCacheMatrix.
## It first checks if the inverse was computed before - in that case it uses the cached data,
## otherwise it performs the inverse computation.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    m
  } else{
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m 
  }
  
}
