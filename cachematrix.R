##Both functions cache the inverse of a matrix.
##makeCacheMatrix - stores and returns input and results
##cacheSolve - if input is new calculates invers of matrix, else returns stored.

#makeCacheMatrix - contains 4 functions.
#get - returns original matrix stored in parent env.
#set - sets new matrix, nullifies inverse matrix stored in parent env.
#setinverse - sets inverse matrix in parent env.
#getinverse - returns inverse of matrix stored. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(invers) inv <<- invers
  getinverse <- function() inv
  list(set=set,get=get,
       setinverse=setinverse, 
       getinverse=getinverse)
}

#cacheSolve - calculates invers of matrix
#if the same matrix inversion was asked - printse inversed matrix stored
#if matrix is new - calculates it, calls setinverse to store result and prints.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
  matr <- x$get()
  inv <- solve(matr)
  x$setinverse(inv)
  inv
  }
}