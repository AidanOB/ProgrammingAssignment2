## These paired functions make an object for the matrix that is given to makeCacheMatrix.
## makeCacheMatrix returns a storage object and initialises the functions associated with it.
## These functions are not run until cacheSolve is called for the first time

## Create an object for the matrix to have special functions surrounding it
makeCacheMatrix <- function(x = matrix()) {
  # Setting m to null
  m <- NULL # This is the variable used for the inverse, it is initially empty
  
  # Forcing the output through to another environment
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  # Declaring the functions that the object can use
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes an object create in makeCacheMatrix and checks if the inverse of the matrix has been created
## If it has been previously generated, it will return the cached matrix rather than recalculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # The matrix was previously calculated, so returns the cached data
  if (!is.null(m)) {
    message('Getting cached data.')
    return(m)
  }
  
  # Getting the initial matrix
  data <- x$get()
  
  # Solving the inverse
  m <- solve(data, ...)
  
  # Setting the inverse to the object
  x$setinverse(m)
  
  # Return the inverse (outputs to screen if calling directly in the console)
  m
  
}
