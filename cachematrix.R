## The following pair of functions will cache the inverse of a matrix.
## The first function is makeCacheMatrix. This function creates a special "matrix" object that can
## cache its inverse.
## The second function is cacheSolve. This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.

## This function,  makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
  
  m <- NULL                  #  m will be our 'inverse' and it is reset to NULL every 
                             #  time makeCacheMatrix is called
  set <- function(y) {       #  takes an input matrix
    x <<- y                  #  assigns the input matrix 
    m <<- NULL               #  resets the inverse to NULL
  }
  get <- function() x        # it returns the value of the original matrix
  
  setsolve <- function(solve) m <<- solve  # this is called by cacheSolve(), it stores the inverse  
  
  getsolve <- function() m    # this will return the cached value to cacheSolve()
                              # on subsequent accesses
  
  list(set = set, get = get,  # This list is returned with the newly created object 
       setsolve = setsolve,   # It lists all the functions that are part of the object
       getsolve = getsolve)   # If a function is not on this list then it cannot be accessed externally
  
}


## This function, cacheSolve, returns a matrix that is the inverse of 'x'. If the inverse has 
## already been calculated, it will be retrieved from cache; otherwise, inverse will be calculated.

cacheSolve <- function(x, ...) {   # the input is an object created by makeCacheMatrix        
  
  m <- x$getsolve()                 # accesses the object 'x' and gets the cached value
  
  if(!is.null(m)) {                 # if inverse was already cached (not NULL)
    message("getting cached data")  # send this message to the console
    return(m)                       # return the cached inverse
  }
  data <- x$get()             # if x$getsolve() returned NULL, then obtain the matrix
  m <- solve(data, ...)       # calculates the inverse
  x$setsolve(m)               # stores the computed inverse in setsolve
  m                           # return the inverse of the matrix  
  
}
