# Creates a special "matrix" object that allows 
# storing a cache of its inverse. The matrix is assumed to
# be invertible
# 
# The object returned implements these functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the 
# special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache 
# and skips the computation. Otherwise, it calculates 
# the inverse of the matrix and sets the value of the inverse 
# in the cache via the setinverse function. The matrix is 
# assumed to be always inversible.
#
# Example: first call to cacheSolve computes the inverse,
# while the second uses the cached inverse matrix.
# 
# > mat <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# > mat$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# > cacheSolve(mat)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > cacheSolve(mat)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # inverse is not cached; calculate and cache it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
