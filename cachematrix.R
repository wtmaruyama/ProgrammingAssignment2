## Put comments here that give an overall description of what your
## functions do

# Creates a list containing a functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinve <- function(inverse) inve <<- inverse
  getinve <- function() inve
  list(set = set, get = get,
       setinve = setinve,
       getinve = getinve)
}


# This function returns the matrix inverse. First, if the inverse matrix is in the cache (ie the inverse matrix has already been computed previously)
# And return this result.
# If it was not calculated, it computes the inverse and set the value in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  # Computing the inverse of a square matrix can be done with the solve function in R.
  # Assume that the matrix supplied is always invertible.
  inve <- solve(data)
  x$setinve(inve)
  inve
}
