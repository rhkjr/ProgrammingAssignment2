## These two functions work together to cache and retrieve the inverse of a matrix.  
## The cached inverse is retrieved through the use of the superassigned variable mat_inv



##*********************************************************************************************************
## makeCacheMatrix
##
## create a matrix with a list of functions that will set, get, set inverse and get inverse of the provided matrix data


makeCacheMatrix <- function(x = numeric()) {
  mat_inv <- NULL

  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(solve) {mat_inv <<- solve}
  getinv <- function() {mat_inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}  
  



##*********************************************************************************************************
## cacheSolve
##
## Determine whether the provided matrix has a cached inverse.  If yes, use it, if no, calculate it.

cacheSolve <- function(x, ...) {
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  mat_inv

}
