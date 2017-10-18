## Programming assignment #2 with comments


## [JC] Creates a matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv
  getInverse <- function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## [JC] The cacheSolve function will compute the inverse of the matrix returned in the above function

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!lis.null(inv)) {          ## getting error here
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
}
