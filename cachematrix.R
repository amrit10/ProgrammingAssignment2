## 'makeCacheMatrix' function creates a special 'list' which contains
## a function to
## - set the matrix
## - get the matrix
## - set the matrix inverse
## - get the matrix inverse
##
## 'cacheSolve' function takes the special 'list' and checks if inverse
## for the same is already calculated and stored in the cache. If found,
## it returns the inverse as it is without calculating again (Saving time
## and computation power). Else, the inverse is calculated again.


## 'makeCacheMatrix' function
##
## 'x' is a invertible matrix whose inverse is to be calculated
##
## Returns a list of required functions, which are used while calculating 
## the matrix inverse
## NOTE: Ensure that the entered matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(w){
    x <<- w
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## 'cacheSolve' function
##
## 'x' is a special list generated for the matrix whose inverse is to be calculated
## through 'makeCacheMatrix' function
##
## Returns the inverted matrix for the entered matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  ## checking if matrix inverse already calculated for the entered matrix, in cache
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  
  ## if not found in cache, calculated
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
