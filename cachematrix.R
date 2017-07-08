## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##initialize inverse_matrix variable will null.
  inverse_matrix <- NULL
  
  ##this function is to set the input variabe to matrix . 
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  ##this function is to get the matrix .
  get <- function() x
  
  ## this function is designed to set the inversed matrix in new variable called inverse_matrix
  setInverse <- function(inverse)  inverse_matrix <<- inverse
  
  ## this function is designed for retrieving the inversed matrix
  getInverse <- function()  inverse_matrix
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  ##check wheather inversed matrix contains a body.
  if (!is.null( inverse_matrix)) {
    message("getting cached data")
    return( inverse_matrix)
  }
  ##If inversed matrix body is equal to null.
  matrix_result <- x$get()
  inverse_matrix <- solve(matrix_result, ...)
  x$setInverse(inverse_matrix)
  
  ##return inversed matrix
  inverse_matrix
}
