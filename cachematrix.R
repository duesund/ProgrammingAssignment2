## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
}


## cacheSolve creates the inverse of the matrix created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinverse(inv_m)
  inv_m
  
}

## Test:

## x <- matrix(rnorm(9),3,3)
## > x
## [,1]       [,2]       [,3]
## [1,]  0.1914453 -0.2003549 -0.7914014
## [2,] -0.4488061 -0.3078116 -0.9110498
## [3,]  0.1862152  1.5331059 -0.0952676
## > m = makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]        [,3]
## [1,]  1.7503163 -1.5126087 -0.07495506
## [2,] -0.2607051  0.1584944  0.65002218
## [3,] -0.7741668 -0.4060354 -0.18269485
## > cacheSolve(m)
## getting cached data
## [,1]       [,2]        [,3]
## [1,]  1.7503163 -1.5126087 -0.07495506
## [2,] -0.2607051  0.1584944  0.65002218
## [3,] -0.7741668 -0.4060354 -0.18269485
## > 