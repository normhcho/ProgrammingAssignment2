## 2 seperate functions 
## This function creates a special "matrix" object that can cache its inverse.

## Writing matrix function

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              
## Setting up matrix
              set <- function(y) {
                x <<- y
                m <<- NULL
              }
## Getting matrix
              get <- function() x
## Setting the inverse
              setInverse <- function(solve) m <<- solve
## Getting the inverse
              getInverse <- function() m
## List of objects
              list(set = set, get = get,
                   setInverse = setInverse,
                   getInverse = getInverse)
}


## Matrix that returns the inverse(solve) of x

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
## If the data is cached or "stored", then returns message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## If no cached value, then getting the values set in x  
  data <- x$get()
  
## Calculating the inverse via solve
  m <- solve(data, ...)
  x$setInverse(m)
  
## The values of the inverse matrix
  m
}

## Testing to see whether the code works
## x <- matrix(data=1:4, nrow=2, ncol=2)  -- A simple 2 X 2 matrix
## foo <- makeCacheMatrix(x) -- Creating a variable that incorporates the values from x and the 
## makeCachematrix function
## bar <- cacheSolve(foo) --To see the inverse data values,then enter bar to the inverted values

## bar <- cacheSolve(foo) --Running it again to see if the "getting cached data" appears

## Would recommend understanding the math of inverse of a matrix to help to verify the code
