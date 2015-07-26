## Caching the Inverse of a Matrix


## makeCacheMatrix function creates a special "matrix" object that can cach its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y;
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv) 
} 


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  ##get inv from cache if it was calculated
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## calculate inverse if it was not calculated
  data <- x$get()
  inv <- solve(data)
  
  ##set inverse value in cache
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}

