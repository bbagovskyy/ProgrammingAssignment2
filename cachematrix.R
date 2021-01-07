## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Set initial value of the inverse matrix
  i <- NULL
  set <- function(y) {
    # Assign the value of the matrix to the x variable of the parent environment
    x <<- y
    # Clear the inverse matrix
    i <<- NULL
  }
  # Return the matrix
  get <- function() x
  # Assign the value to the parent environment
  setinverse <- function(solve) i <<- solve
  # Return the cached inverse matrix
  getinverse <- function() i
  # Return the list of functions by names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: calculates the mean of the special "vector" created with the 
## makeCacheMatrixfunction

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Get the inverse matrix
  i <- x$getinverse()
  ## If it is not NULL the inverse has already been calculated; skip calculations
  ## and return the cached matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## Solve the matrix
  i <- solve(data, ...)
  ## Cache the inverse
  x$setinverse(i)
  # Return the inverse
  i
}
