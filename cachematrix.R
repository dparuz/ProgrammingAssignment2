## Cache the inverse of a matrix

## A function that creates a matrix that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
#Set the matrix
  
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  #Get the matrix
  get <- function() x
  
  #Set the inverse
  setinv <- function(inverse) m <<- inverse
  
  #Get the inverse
  
  getinv <- function() m
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)

}



## Compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  # Return the inverse if it's calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Calculate the inverse if it's not set
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



