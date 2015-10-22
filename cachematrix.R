## Cache Matrix inversion functions rather than compute it repeatedly
## Will write pair of functions to cache inverse of a matrix

## makeCacheMatrix will: 
## a. set the value of the matrix
## b. get the value of the matrix
## c. get value of inverse of the matrix
## d. set the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL

set <- function(y){
  x<<- y
  inv <<- NULL
  
}

  get <- function()x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse )
}
  


## cacheSolve computes the inverse of the matrix returned by makeCahceMatrix. 
## if the inverse is already cached, it will retrieve result from memory directly 
## via the function, instead 
## of calcuating. If not, inverse is calculated via setinverse function. 

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if (!is.null(inv)){
    message("Cached data from memory will be fetched...")
    return(inv)
    
  }
  data<-x$get()
  inv <-solve(data)
  x$setinverse(inv)
  inv
  
}
