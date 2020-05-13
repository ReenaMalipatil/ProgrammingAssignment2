
## These are a group of functions used to create cache of matrix inverse values. A matrix inverse 
## is calculated(using the solve function) and stored in the environment for easier retrieval using 
## lexical scoping principles


## This function creates a special "matrix" which basically returns a list containing 4 functions -
## set (sets the value to the provided value), get (retrieves a value), 
## setinverse (sets the inverse value obtained), getinverse (gets the inverse value)


makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y)  {
        x <<- y
        s <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inv) s <<- inv
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function checks if the inverse of the provided matrix is available. If available, the 
## computation is skipped and the cached value is returned. Otherwise, the inverse of the matrix is 
## computed using the solve function and returned

cacheSolve <- function(x, ...) {
      s <- x$getinverse()
      if(!is.null(s)) {
        message("getting cached data")
        return(s)
      }
      
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
  
}

