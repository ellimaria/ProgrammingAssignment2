## These two functions compute the inverse of square matrices
## their usage avoids computing the same inverse twice, as an already calculated matrix
## is stored and can be used from storage

## makeCacheMatrix creates variable m, 
## and creates functions set, get, setinverse, getinverse and names them

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve 
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## cacheSolve checks whether m is null, 
## if it is, an inverse is computed
## if it isn't, an inverse is returned from cache

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      
      if(!is.null(m)) {
            message("getting cached data") 
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
      ## Return a matrix that is the inverse of 'x'
}