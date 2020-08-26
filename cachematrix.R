## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that creates and saves the matrix 
## along with its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Variable that saves the result of the mean
  ## At first, it has a value of 'NULL' because
  ## it's creating the vector with all of his attributes, not calculating the inverse yet
  m <- NULL
  ## set function that is responsible to set a new value, a new matrix for the variable
  set <- function(y){
    ## Assign the value of 'y' to 'x' 
    x <<- y
    ## Assign the value of 'NULL' to 'm' 
    m <<- NULL
  }
  ## get function that is responsible to return 'x' which its the matrix
  get <- function() x
  ## setcache function that is responsible to set the value of the inverse to 'm' 
  ## which its the variable that cacheSolve checks if has a value already assigned
  setcache <- function(invmatrix) m <<- invmatrix
  ##getcache function is responsible to return the value of m, which is the inverse
  getcache <- function() m
  ## When displaying the values of the variables, its gonna return a list with
  ## this attributes with the information already process above
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}

## Write a short comment describing this function
## cacheSolve is a function that first checks whether the variables has already been 
## process if not it calculates the inverse and then assign the value to the varibale
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Getting the value of the inverse, if there's no value it will return a 'NULL'
  m <- x$getcache()
  ## Condition that checks if the m has a valid value or a 'NULL' value
  if(!is.null(m)) {
    ## Printing a message 
    message("getting cached data")
    ## Returning the inverse of the variable
    return(m)
  }
  ## Data will receive the matrix from the variable
  data <- x$get()
  ## Variable 'm' will store the value of the solve function
  m <- solve(data, ...)
  ## Set the value of the inverse which is 'm' to the cache of the variable
  x$setcache(m)
  ## Returns the inverse of the matrix
  m
}
