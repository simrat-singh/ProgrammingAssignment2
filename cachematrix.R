#These functions checks for cached inversed matrix for the input matrix.
#If the inversed matrix exists it displays the value else it inverse the
#matrix and cache it.


#This function creates a list of objects to be consumed and also retain cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set <- function(inv_data) inv <<- inv_data
  getinv <- function() inv
  list(set = set, get = get, setinv = set, getinv = getinv)
}

#This function inverses the matrix and also set cache if no data is cached
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data)
  x$setinv(m)
  m
}

#Test cases
#library(datasets)
#iris_subset<-iris[1:4,1:4]
#m<-makeCacheMatrix(iris_subset)
#cacheSolve(m)