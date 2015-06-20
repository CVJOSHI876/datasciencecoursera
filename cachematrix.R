## Matrix inversion is potentially time-consuming computations.It makes sense to
## cache the inverse instead of computing it repeatedly when the contents of matrix
## are not changed.Following functions are used to cache the inverse of the matrix.


## The function creates a matrix object that can cache the inverse. Function creates
## a list containing a functions to set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(i) {
    inv <<- i
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Below function calculates the inverse of the matrix returned by makeCacheMatrix 
## function above.The function first checks if the inverse has already been computed
## for the given marix,then it returns the inverse from the cache, else it calculates
## the inverse and sets in the cache. 



cacheSolve <- function(x, ...) {
       
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("retrieving from cache")
    return(inv)
  }
  ## calculate the inverse for the very first time
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##  If you want to test the functions, uncomment code below and run

##  mx = rbind(c(1, -1/4), c(-1/4, 1))
##  m = makeCacheMatrix(mx)
##  m$get()
##  cacheSolve(m)
