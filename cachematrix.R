## Two functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)
    inv<<-inverse
  getInverse<-function()inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}

## To compute the inverse of the special matrix object returned by makeCachematrix by 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(matrix,...)
  x$setInverse(inv)
  inv
}
