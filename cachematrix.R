## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() x
  setinvers <- function(inv) inv_matrix <<- inv
  getinvers <- function() inv_matrix
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- makeCacheMatrix(x)$getinvers()
  
  if(!is.null(inv_matrix)){
    message("Returning cached inversion")
    return(inv_matrix)
  }
  
  data <- makeCacheMatrix(x)$get()
  inv_matrix <- solve(data, ...)
  makeCacheMatrix(x)$setinvers(inv_matrix)
  
  inv_matrix
}
