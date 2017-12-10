##Functions for programming assignement 2 - lexical scoping

## Cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv01 <- NULL   ##initialise
  set01 <- function(y){    ##create "copies"
    x <<- y
    inv01 <<- NULL
  }
  get01 <- function() x
  set_Inv01 <- function(solveMatrix)
  inv01 <<- solveMatrix
  get_Inv01 <- function() inv01
  list(set01 = set01, get01 = get01, set_Inv01 = set_Inv01, get_Inv01 = get_Inv01) ## create the cached matrix
}


## return the inverse of the matrix that was cached in the previous function

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  inv01 <- x$get_Inv01()
  if(!is.null(inv01)){
    message("Retrieving Cache...")
    return(inv01)
  }
  data02 <- x$get01()
  inv01 <- solve(data02)
  x$set_Inv01(inv01)
  inv01  ##return the inverse  
}
