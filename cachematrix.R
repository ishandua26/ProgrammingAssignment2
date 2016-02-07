## the following two functions are used to cache the inververse of a matrix.

# makeCacheMatrix creates a list containing a function which sets the value of the matrix
# and the value of inverse of the matrix and gets the value as well.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# here the cacheSolve first checks if the inverse has already been computed. 
# If so, it gets the result and skips inverse part and if not, it computes the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
