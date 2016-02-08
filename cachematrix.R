## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        

  inverse <- x$getinverse()
  
  # if the inverse has already been calculated
  if(!is.null(inverse)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  inverse <- solve(data)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(inverse)
  inverse
}

