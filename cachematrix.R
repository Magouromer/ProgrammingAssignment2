## Put comments here that give an overall description of what your
## functions do

# The first function creates a matrix object that caches its inverse. The second one computes the inverse of the matrix based on the previous function.
# If the inverse has already been calculated, then the second function will retrieve the inverse from the cache. At the end of the code, there is an application example.

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.
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


## Write a short comment describing this function

# This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.
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
  
}

# Application example

m1 <- makeCacheMatrix(matrix(seq(1,4,1), nrow = 2, byrow = T))

# Have a look to matrix m1
m1$get()

#Check if m1 is an inversible matrix (det (m1) != 0)
det(matrix(seq(1,4,1), nrow = 2, byrow = T))

# Get the inverse of m1
cacheSolve(m1)
