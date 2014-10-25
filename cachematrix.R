## makeCacheMatrix and cacheSolve functions essentially caches the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is a list of functions: 
## 1) set (set) the value of the matrix and NULLs the cache such that the inverse 
##    may get computed in the cacheSolve function
## 2) get (get) the value of the matrix 
## 3) set (setinverse) the value of the inverse of the matrix
## 4) get (getinverse) the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_cache <- NULL
  set <- function(y) {
    x <<- y
    inverse_cache <<- NULL
  }
  get <- function () {
    x
  }
  setinverse <- function(inverse) {
    inverse_cache <<- inverse
  }  
  getinverse <- function () {
    inverse_cache
  }
  #create a list of the functions listed above
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## cacheSolve first checkes whether the inverse of the matrix has already been calculated
## If yes, it gets the inverse from the cache and skips the computation
## If not, it gets the matrix, calculates the inverse of the matrix, and sets the value of 
##  the inverse in the cache through the setinverse function.

cacheSolve <- function(x, ...) {
  inverse_cache <- x$getinverse()
  if(!is.null(inverse_cache)) {
    message("getting cached data")
    return(inverse_cache)
  }
  matrix <- x$get()
  inverse_cache <- solve(matrix, ...)
  x$setinverse(inverse_cache)
  inverse_cache
}