## This is a function to cache potentially time consuming computations

## This function creates a cacheable special matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function creates an inverse of MakeCacheMatrix function above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.Null(inv)){
    message("return cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

}
