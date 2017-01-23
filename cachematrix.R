## The first function creates the matrix to be inverted.
## The second matrix either inverts the matrix, or returns cahced data.

## To get the second function to work, this must be a square matrix.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function gets cached inverse matrix data or inverts the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
