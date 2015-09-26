## This script calculate the inverse of a Matrix and store it in the cache


## The next function store a matrix to calculate it inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## That function calculates the inverse of the matrix stiored in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinv()
  
  if(!is.null(inverse)) {
    
    message("getting cached data")
    return(inverse)
    
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
  
}
