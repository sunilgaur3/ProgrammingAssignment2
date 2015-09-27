## The function Caches the Inverse of a Matrix
## Below functions stores the matrix and caches its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv<<- inverse
  getinv<- function() inv
  list( set= set, get= get,
        setinv= setinv,
        getinv= getinv)
}

## the function computes the inverse of the specical Matrix created by the above
## function. If the inverse has already been calculated, then it retrieves the 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinv(inv)
  inv
}
