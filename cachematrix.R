#Two functions to cache and compute the inverse of
#a square matrix.

makeCachceMatrix <- function(mat = numeric()) {
  #Sets inv to NULL in case something else was already stored there.
  inv <- NULL
  
  set <- function(y) {
    #Used to set a new matrix to mat and clear inv to NULL.
    mat <<- y
    inv <<- NULL
  }
  
  get <- function() {
    #Used to fetch the current matrix in mat.
    mat
  } 
  
  setInverse <- function(inverse){
    #Used to set a matrix to inv. Note: Does NOT compute the inverse.
    inv <<- inverse
  } 
  
  getInverse <- function() {
    #Used to fetch the current value of inv.
    inv
  }
  
  #Creates a list of options for makeCachceMatrix.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}




cacheSolve <- function(x, ...) {
  #Retrieves the current value of inv for the provided matrix. 
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    #If the inv value has been preivously cached,
    #it fetches the value and returns it. 
    message("getting cached data")
    return(inv)
  }
  
  #Assigns the current matrix to data. 
  data <- x$get()
  
  #Computes the inverse of the current matrix.
  inv <- solve(data, ...)
  
  #Caches the inverse of the matrix for future use.
  x$setInverse(inv)
  
  #Returns the inverse of the matrix.
  inv
}



cacheSolve <- function(x, ...) {
  #Retrieves the current value of inv for the provided matrix. 
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    #If the inv value has been preivously cached,
    #it fetches the value and returns it. 
    message("getting cached data")
    return(inv)
  }
  
  #Assigns the current matrix to data. 
  data <- x$get()
  
  #Computes the inverse of the current matrix.
  inv <- solve(data, ...)
  
  #Caches the inverse of the matrix for future use.
  x$setInverse(inv)
  
  #Returns the inverse of the matrix.
  inv
}
