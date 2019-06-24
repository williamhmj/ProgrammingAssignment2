## The makeCacheMartrix function is to create a special 'matrix' to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
   set <- function(y){
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <- inverse
  getinverse <- function() n
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function is to compute the inverse of the special 'matrix'
## returned by makeCacheMatrix
## if the inverse has been calcualted from the above 'matrix',
## the cacheSolve function should be able get the inverse from the cache.
## Otherwise, the cacheSolve should be able to calculate the inverse of the above 'matrix'.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)){
    message("getting the cache data")
    return(n)
  }
  data <- x$get()
  n <- solve(data,...)
  x$setinverse(n)
  n
        ## Return a matrix that is the inverse of 'x'
}
