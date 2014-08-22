## The functions makeCacheMatrix and cacheSolve work together to provide 
## the functionality to keep the inverse of a matrix once calculates 
## in a cache. This is done in order to save time when a complex matrix
## has many opearations that don't modify itself comparing with updates. 

## makeCacheMtrix is a function that works like an object for the
## set, get, setInverse and getInverse operations - this function keeps the
## cache for the inverted matrix.

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


## The cacheSolve function receives a matrix which is an instance of
## makeCacheMatrix and returns the inverse matrix. If the inverse matrix is cached, this
## function just return what was in the cache. Otherwise, the solve function is used to
## calculate the inverse matrix. In the later case, th inverse matrix is set to 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'debug
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
