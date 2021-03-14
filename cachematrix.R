## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its 
## inverse.

## This function follows the structuring of the makeVector example:
## it first initializes two objects, x and s, x is initialized as 
## the function argument and s is initialized as NULL; next it 
## defines the set, get, setinverse, getinverse functions, using 
## the approach that in the "setters" we assign RHS objects using 
## objects defined in the parent environment (this enables mutating
## from the original matrix values), with the "getters" also using lexical
## scoping to refer back to the parent environment; last we create a 
## new object assigning each of these functions as elements w/n a list, 
## naming list elements to enable more obvious calling

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

## This function follows the structuring of the cachemean() example:
## it attempts to retrieve previously calculated values for s, if they 
## exist, else calculates s using the setinverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
