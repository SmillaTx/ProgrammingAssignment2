## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## This function creates a "matrix" that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
 + IV<-NULL                           # initialize IV as NULL
 + set<-function(y){                  # sets function to assign new 
 + x<<-y                              # matrix in parent environment 
 + IV<<-NULL                          # set IV to NULL if new matrix 
 }
 
 + get<-function()x                   # returns value of the matrix 
 + setIV<-function(inverse)IV<<-inverse # value of IV in parent environment 
 + getIV<-function()IV                  # value of IV 
 + list(set=set,get=get,setIV=setIV,getIV=getIV)  
}


## Write a short comment describing this function
# This function calculates the inverse of the "matrix" returned from the function above.
# If the matrix already has been calculated then this function will call from the cache. 

cacheSolve <- function(x, ...) {
 + IV<-x$getIV()
 + if(is.null(IV)){
   message("cached data")
   return(IV)
 }
 + dataset<-x$get()
 + IV<-solve(dataset,...)
 + x$setIV(IV)
 + IV
}

