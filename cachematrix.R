## Comments about makeCacheMatrix function:
## begins by setting the special matrix to NULL as a placeholder for a future value.
## defines a function to set the matrix, x, to a new matrix, y, and resets the solve, s, to NULL.
## returns the matrix, x.
## solve sets the solve, s, to solve.
## function() s returns the solve, s.
## returns the 'special vector' containing all of the functions just defined.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL 
  set <- function(y) {
    x <<- y
    s <<- NULL 
    
  }
  get <- function() x 
  setinverse<- function(solve) s <<-solve 
  getinverse <- function() s 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}

## Comments about cacheSolve function:
## CacheSolve calculates the inverse of the special matrix, created with makecachematrix function.
## First it checks if the inverse has already been calculated. If so, it gets the inverse from the cache and show the message "getting cached inverse matrix".
## Otherwise it calculates the inverse of the data and gets the result of the inverse in the cache via the getinverse function.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if (!is.null(s)) {
    message("getting cached inverse matrix")
    return(s)
    
  } 
  data <- x$get ()
  s <- solve(data, ...)
  x$setinverse(s)
  return(s)
}

## To test the functions:
## m1 <- matrix (c(4,2,3,5),2,2)
## m2 <- matrix (c(8,7,6,1),2,2)
## i1 <- makeCacheMatrix(m1)
## i2 <- makeCacheMatrix(m2)
## cacheSolve(i1)
## cacheSolve(i2)
## cacheSolve(i1)
## cacheSolve(i2)

  