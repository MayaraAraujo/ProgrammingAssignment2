## Put comments here that give an overall description of what your
## functions do
## makeCachedMatrix returns an object to store the matrix and its inverse
## cacheSolve calculates the inverse 


## Write a short comment describing this function
## makeCacheMatrix returns a special object containing a matrix, a cached value for its inverse and some functions to interact with its values

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve at first checks to see if the cached value is null or not, if not it returns the cached value
## if it is null, it calculates the inverse through solve() , saves it to the cache and returns it 

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
