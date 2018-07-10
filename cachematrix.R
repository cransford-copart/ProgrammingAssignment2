
getwd()
setwd("C:/Users/cjransford/datasciencecoursera/rprogramming_assignment2/ProgrammingAssignment2")

## The 'makeCacheMatrix' function creates space to store an object. It does this by first assigning the input matrix (x) to the 
## parent environment, and then clears out any value stored to m in the parent value. This allows the function to later compare the 
## the recalculated inverse matrix to the cached matrix. 

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

## The cacheSolve function checks our cache (m) to determine if a value is present. If it does not return empty (!is.null), then the 
## funciton prompts the user with a note indicating "getting cached data", and retrieves the cached matrix from the cache (m). 
## If the system does return empty, then the solve() function is executed, and the returned value (inverse matrix) is set to cache (m)

cacheSolve <- function(x, ...) {
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



## Examples 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1) 
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)








