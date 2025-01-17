## These functionc create a matrix and calculates its inverse which can be cached

## This function creates a matrix and can get the inverse value if previously cached
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y = matrix()) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix if not previously cached
cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      return(i)
}