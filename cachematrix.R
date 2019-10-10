## This is the function that creates an object (list) per matrix, that caches the inverse if computed once.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y){
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function() i
  list(setmatrix = setmatrix, getmatrix=getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a matrix. If the matrix has already been passed, the inverse is recalled from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  mat = x$getmatrix()
  inverse = solve(mat)
  x$setinverse(inverse)
  inverse
}
