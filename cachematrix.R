## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL ## initialize the inverse property 
  set <- function(y) { ## method to set the matrix
    x <<- y
    inversa <<- NULL
  }
  get <- function() x ##method the get the matrix 
  setInversa <- function(inversacalculada) inversa <<- inversacalculada ##method to set the inverse of the matrix
  getInversa <- function() inversa ##method to get the inverse of the matrix  / return the inverse property 
  list(set = set, get = get,  ##return a list of the methods
       setInversa = setInversa,
       getInversa = getInversa)
}



cachesolve <- function(x, ...) {
  inversa <- x$getInversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get() ##get the matrix from our object
  inversa <- solve(data, ...) ## Calculate the inverse using matrix multiplication
  x$setInversa(inversa) ## set the inverse to the objects
  inversa ## Return a matrix that is the inverse of 'x'
}
        
