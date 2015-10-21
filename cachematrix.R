## Function 1 takes a matrix as an input caches the value 
## of the matrix and outputs a list of functions
## Functions 2 checks for the existence of a cached inverse of 
## the matrix and returns the cached value if exists otherwise creates
## the inverse. 

## This function creates a list of functions that can be called 
## using the cached values of x and m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## 1. This function takes a matrix as it's input 
## 2. Checks to see if the inverse of the matrix has already been cached
## 3. If the inverse of the matrix is already cached it returns the cached data
## 4. If it is not in the cache it inverts the matrix using solve 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
