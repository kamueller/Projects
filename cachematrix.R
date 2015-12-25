## My functions compute cache the value of an inverse matrix, so that if the same is needed again, it needn't to be computed again

## makeCacheMatrix caches the value of the matrix
# stores a list of four functions 
# set: changes the matrix x stored in the main function makeCacheMatrix
# get: returns matrix x stored in main function
# setInver: stores the value of the input in a variable into the main function makeCacheMatrix 
# getInver: returns the value of input in a variable into main function

makeCacheMatrix <- function(x = numeric()){
  i  <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInver <- function(inver) i <<- inver
  getInver <- function() i
  list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## cacheSolve compute the inverse of a matrix 
# Input of cacheSolve is the object where makeCacheMatrix is stored

cacheSolve <- function(x,...) {
  i  <- x$getInver()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInver(i)
  i
}

