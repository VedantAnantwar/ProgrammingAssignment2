

## This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invs<-NULL;
  set<-function(y){
    x<<-y
    invs<-NULL
  }
  get<-function() x
  setinverse<-function(inverse_mat) m<<-inverse_mat;
  getinverse<-function() invs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setinverse(invs)
  invs
  
}