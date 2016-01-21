
## This function is used to get list which is used in calculating inverse of matrix.
# It returns following list of functions
# set := set matrix data as per argument and null to inverse of matrix
# get := get the matrix data
# setinv := set inverse as per argument
# getinv := return inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invrs = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invrs <<- NULL
  }
  get = function() x
  setinv = function(inverse) invrs <<- inverse 
  getinv = function() invrs
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## calculate inverse and set in cache
# This function returns inverse of matrix if its already calculated otherwise calculates inverse and store it.
cacheSolve <- function(x, ...) {
  
  invrs = x$getinv()
  # check if inverse already calculated if yes then return it.
  if (!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  
#calculate, store and return inverse.
  data = x$get()
  invrs = solve(data, ...)
  
  x$setinv(invrs)
  
  return(invrs)
}
