## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #inv to hold value of matrix inverse
  set<-function(y){
    x<<-y   #value of matrix in parent environment
    inv <<- NULL
  }
  get<-function(x)   #get function to return value of matrix argument
    setinverse<-function(inverse)   #assign value of inv in parent environment
      inv<<- inverse
  getinverse<-function()inv  #get value of inv
  list(set = set, get=get, setinverse= setinverse, getinverse= getinverse)
}




## Write a short comment describing this function
#This function computes inverse of special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
  
