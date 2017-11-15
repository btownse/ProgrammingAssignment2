# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly  

# MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inverse<-NULL
  set<-function (y) {
    x<<-y
    inverse<<-NULL
  }
  get<-function(){
    x
  } 
  setinverse<-function(m) inverse<<-m
  getinverse<-function()inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
# already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message ("receive cached data")
    return(inverse)
  }
  data<-x$receive()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse         
}
