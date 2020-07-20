## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function is creating a special object that will contain matrix and its inverse
## the result obtained after calling makeCachematrix must be stored in a variable which should be passed as argument to cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##this function will either compute or retrive data from the makeCachematrix object and even calculate inverse if it is not yet calculated
cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)){
    print("getting cached data")
    return(i)
  }
  data <- x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
