##The following functions establish the values of a matrix and inverse and estimate the latter,
  # after ensuring that it does not already exist. 

##makeCacheMatrix defines the values of a matrix and  inverse (using a function named "set"), 
  #makes these values accessable (via a function named "get"), and stores the aformentioned 
  #objects in a list. 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL    
  }
  get<-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv, getinv=getinv)
}

## cacheSolve estimates and displays the inverse of the matrix set by makeCacheMatrix, 
 # it does this after verifying that this value does not already exist. If the value 
 #does exist, cacheSolve displays a message ("getting cached data"), retrives the existing
 #inverse value and displays it in turn. 

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
