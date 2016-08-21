## creating 2 main makeCacheMatrix and cacheSolve to get inverse of matrix
## Below is the description of each function

## Function "makeCacheMatrix" creates a "matrix" object that will cache its inverse.
##makeCacheMatrix contains 4 functions: set, get, setmean, getmean.

makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <-function(y){
    x <<- y
    inverse<<- NULL
  }

  get <-function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
 

### checking function
a<-diag(2,2,2)
a

CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)