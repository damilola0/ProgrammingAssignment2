## My pair of functions cache the inverse of a matrix

## The following function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
               m <- NULL
               setMatrix <- function(y) {
               x <<- y
               m <<- NULL
  }
               getMatrix <- function() x
               setinverse <- function(inverse) m <<- inverse
                getinverse <- function()m
          list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse,
       getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix above. If the inverse has already been calculated(and 
## the matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <<- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$getMatrix()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## Test

## > X <- matrix(8:11, c(2,2))
## > X
##      [,1] [,2]
## [1,]    8   10
## [2,]    9   11
## > Y <- makeCacheMatrix(X)
## > Z <- cacheSolve(Y)
## > Z
##      [,1] [,2]
## [1,] -5.5    5
## [2,]  4.5   -4