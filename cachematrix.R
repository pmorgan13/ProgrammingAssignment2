## Put comments here that give an overall description of what your
## functions do

# My functions cache the inverse of a matrix. The first function creates the list of function and
# the second function finds the inverse of the matrix by first checking if it is cached

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y){
            x <<- y
            invMat <<- NULL
      }
      get <- function() x
      setInv <- function(Inv) invMat <<- Inv
      getInv <- function() invMat
      list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInv()
      if (!is.null(invMat)){
            message('getting cached data')
            return(invMat)
      }
      data <- x$get()
      invMat <- solve(data,...)
      x$setInv(invMat)
      invMat
}
