## Creates a cached matrix and then gets the inverse of cached matrix


## This function creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the above matrix

CacheSolve<-function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

