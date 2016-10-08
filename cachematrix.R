##
## create a matrix to have the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##
## calculates the inverse of the matrix
## if the result already in the memory them return
## solve the matrix
##
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}

## mymatrix <- makeCacheMatrix(matrix(c(1,1,4, 0,3,1, 4,4,0), nrow =3))
##
## cacheSolve(mymatrix)

