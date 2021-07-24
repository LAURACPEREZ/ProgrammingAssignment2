## Put comments here that give an overall description of what your
## functions do

## In this experiment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <-  function(y){
    x<<-y
    inversa<<-NULL
  }
  get <- function(){x}
  setInverse <- function(inversaCalc) {
    inversa <<- inversaCalc
  }
  getInverse <- function() {inversa}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
x<- makeCacheMatrix (matrix(1:4, nrow= 2, ncol=2))
x
x$get()

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the 
##special “matrix” returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inversa<- x$getInverse()
  if( !is.null(inversa) ) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data,...)
  x$setInverse(inversa)
  inversa
}
cacheSolve(x)
