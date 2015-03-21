## Assignment 2 - Cache the inverse of a matrix
## If no inverse found in cache, invert the matrix inputted
## Return the inverse

##Input a matrix, convert to a special "matrix" that caches itself; 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                  
  set <- function(y){
       x <<- y
       m <<- NULL
  }
  get <- function()x
  
##compute the inverse of the matrix & assign to m
  setinv <- function(solve(x)) m <<- solve()      
  getinv <- function() m                          
  list(set = set, get = get, 
        setinv = setinv, getinv = getinv)
}

## Check for a cached inverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
        message("getting cached data")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
  
  
}
