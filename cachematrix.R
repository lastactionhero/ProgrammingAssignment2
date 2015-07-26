
## This function is used to get list which is used in calculating inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  invrs = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) invrs <<- inverse 
  getinv = function() invrs
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## calculate inverse and set in cache

cacheSolve <- function(x, ...) {
  
  invrs = x$getinv()
  
  if (!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  

  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(invrs)
  
  return(invrs)
}
