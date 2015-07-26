## Programming Assignment 2: Lexical Scoping
##    Caching the Inverse of a Matrix: functions help 
##    simplify computation of a matrix inverse using 
##    a cache.

## makeCacheMatrix: a function to create a cache for
## storing the values of an inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##create functions
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  ##create an object containing functions
  list(set=set, get=get,setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## cacheSolve: a function to compute the inverse of a
## matrix, using the object created in makeCahceMatrix
## in order to reduce computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  ## Check the cache.
  if(!is.null(m)){
    message("Getting cached data.")
    return(m)
  }
  matrix<-x$get()
  ## m will store solution
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
