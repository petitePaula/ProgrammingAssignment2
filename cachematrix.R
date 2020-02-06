## Both functions written below store the inverse of a matrix. 

## The function makeCacheMatrix creates a special object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
j <-NULL
set<-function (y){
  x<<-y
  j<<-NULL
}
get<-function()x
setInverse<-function(inverse)j<<-inverse
getInverse<-function()j
list(set=set, 
     get=get, 
     setInverse=setInverse, 
     getInverse=getInverse)
}


## The function named cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   j<-x$getInverse()
   if(!is.null(j)){
     message ("getting cached data")
     return(j)
   }
  mat<-x$get()
  j<-solve(mat,...)
  x$setInverse(j)
  j
}
