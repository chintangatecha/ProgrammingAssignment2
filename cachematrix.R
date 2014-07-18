#following function makes a matrix allows you to save permanently into a variable
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

#Following function will first check if the inverse exists, and if not 
# it will calculate and save the matrix for future use
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #returns the matrix from the cached variable
  }
  data <- x$get() #getting the data and feeding to the data variable.
  m <- solve(data, ...)  #inbuilt method to calculate the inverse
  x$setinverse(m)  #calls the method to set the matrix for re use
  m  #returns the matrix
}
