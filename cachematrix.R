##Matrix inversion is usually a costly computation and their may be some benefit 
##"to caching the inverse of a matrix rather than compute it repeatedly 

##The "makeCacheMatrix" function is creates a special 
##"matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y){ 
    x <<- y    
    m <<- NULL
  }
  get <-function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The "cacheSolve" function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) { 
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()   ##This was used get the matrix as a vector.
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}