# cachematrix.R
# https://github.com/briansuk/ProgrammingAssignment2
# December 15, 2014
# Contains two functions to complete the second programming assignment.
# makeCacheMatrix, cacheSolve

# Function makeCacheMatrix is an object container that stores the matrix data and
# contains setters and getters.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize an empty inverse.
  inv <- NULL
  
  # Setter. Input is a new matrix, and it also clears out the cached inverse.
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # Getter, matrix data.
  get <- function(){
    x
  }
  
  # Setter for the inverse cache.
  setInverse <- function(newInverse){
    inv <<- newInverse
  }
  
  # Getter, inverse cache.
  getInverse <- function(){
    inv
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


# Function cacheSolve contains the functions to store, calculate, and retrieve the inverse
# of the matrix, and its cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  # Retrieves pointer for cached matrix, skips if empty.
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  # Get the data, solve the inverse, set the inverse cache, return the inverse.
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}