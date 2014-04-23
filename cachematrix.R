## This program computes the inverse matrix of a matrix and cache its inverse
## to reduce computation time. It also makes sure to compute a new inverse when
## the input matrix is changed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  iM <- NULL              # set inverse Matrix to NULL 
  set <- function(y) {
    x <<- y               # set the input Matrix
    iM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseM) iM <<- inverseM
  getinverse <- function() iM
  
  # the function returns the list of functions
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the
## cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  iM <- x$getinverse()  # get the inverse matrix
  sqDim<-nrow(x$get())
  
  # check if there is an inverse matrix cached
  
  if(!is.null(iM)) {
    
    #check to see if the input matrix has'nt changed
    identityM<-diag(sqDim)
    toCheckIdentityM<-x$get() %*% iM

    if (identical(toCheckIdentityM,identityM)) {
      
      message("getting cached data")
      return(iM)                   # return the cached inverse Matrix 
    }
    
  }
  
  data <- x$get()
  iM <- solve(data) #compute inverse matrix
  x$setinverse(iM) # cache the inverse matrix
  iM # return the computed inverse Matrix
}
