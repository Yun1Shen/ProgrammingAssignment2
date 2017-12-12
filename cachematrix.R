## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function is a list containing a function to
## set and get the value of the matrix, to set and get the 
## inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  set<-function(y){
   x<<-y
   invm<<-NULL
  }
  get<-function() x
  setinv <-function(invx) invm<<-invx
  getinv <-function() invm
  list(set=set,get=get,
       setinv=setinv,
       getinv = getinv
  )

}


## Write a short comment describing this function
## The following function calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm<- x$getinv()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  data<-x$get()
  invm<-solve(data,...)
  x$setinv(invm)
  invm
       
}
