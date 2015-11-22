makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x <<- y
    f <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) f <<- inv
  getinverse <- function() f
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  f <- x$getinverse()
  if(!is.null(f)) {
    message("getting cached data")
    return(f)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


