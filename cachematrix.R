## ## To caching the inverse of a matrix rather than compute the matrix repeatedly

## The makeCacheMatrix function returns a list containing a function to 
## get the value of matrix
## set the value of matrix
## get the value of the inverse of the matrix
## set the value of the inverse of the matrix

## set value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## get value of the matrix
  get <- function() x
  ## set value of the inverse of the matrix    
  setInverse <- function (inverse) inv <<- inverse
  ## get value of the inverse of the matrix  
  getInverse <- function () inv
  ## makeCacheMatrix returns four functions   
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x) {
  inv <- x$getInverse()
  ## check if the inverse of the matrix was calculated or not.  If yes, get the result 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if no inverse of the matrix was calcuated, cacheSolve will get the matrix from cache
  ## then calculate the inverse of the matrix
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

m <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)

l <- makeCacheMatrix(m)
cacheSolve(l)

