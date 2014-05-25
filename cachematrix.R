## This function creates a special "matrix" object that can cache its inverse.
## It will find it in the cache and return it, if inverse "matrix" object has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrice<-function(solve) m <<- solve
  getmatrice<-function() m
  list(set = set, get = get,
       setmatrice = setmatrice,
       getmatrice = getmatrice)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrice()
  if(!is.null(m)) {
    message("getting cached matrice")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrice(m)
  m
}
  
