## cacheSolve is a function that will search for a matrix inverse in cache
## and return the result if it is found. If the result is not found, it
## will use solve to get the inverse and then store it in cache.

## Get the inverse of a matrix and store it in cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #use solve to get inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of a matrix, either from cache or using solve
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #if inverse is found, retrieve from cache instead of computing
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()  #get the matrix 
  m <- solve(data) #get inverse of matrix
  x$setinverse(m)  #cache the result
  m
}
