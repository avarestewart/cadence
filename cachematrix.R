#' The following functions work together to cache potentially
#' time consuming computations for the matrix inverse operations.
#' The makeCacheMatrix takes advantage of the 
#' scoping rules of the R language regarding closure
#' allowing the state of the inverse to be preserved across 
#' invocations. The function cacheSolve  retrieve the inverse
#' from the cache.
#' Make an object to cache matrix inverse
#'
#' @param x invertible square matrix
#'
#' @return a list containing a function to: 1) set the value
#' of the matrix
#' 2) get the value of the matrix; 3) set the inverse of the
#'  matrix; and 4) get the inverse of the matrix
#' @export
#'
#' @examples
#' source("cachematrix.R")
#' amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#'  amatrix$get()         # Returns original matrix
#'  [,1] [,2]
#'  [1,]    1    3
#'  [2,]    2    4
makeCacheMatrix <- function(x = matrix()) {

  # ensure matrix
  if (!is.matrix(x))
    stop("Argument supplied must be a matrix!")
  
  # ensure matrix is square
  if (!identical(nrow(x), ncol(x))) {
    #print(Dimdim(x))
    stop( "Check argument dimensions. square matrix required.")
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#' Compute inverse of the special "matrix" returned by 
#' makeCacheMatrix. If the inverse has already been 
#' calculated (and the matrix has not been cached), then the 
#' function cacheSolve should retrieve the inverse from 
#' the cache.
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#'  >   cacheSolve(amatrix)   # Computes, caches, and returns    matrix
#'  inverse
#'  [,1] [,2]
#'  [1,]   -2  1.5
#'  [2,]    1 -0.5
#'  >  amatrix$getinverse()  # Returns matrix inverse
#'  [,1] [,2]
#'  [1,]   -2  1.5
#'  [2,]    1 -0.5
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
