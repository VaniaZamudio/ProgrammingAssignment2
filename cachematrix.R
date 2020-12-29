## We want to return the inverse of a matrix, so we 
## are going to describe a pair of functions to do that

## This function will save in makeCacheMatrix the values of a matrix
## we specified at first 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) 
    i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve will get the inverse of the function with all the data set before

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

##Here we checked our function if it´s really good
Matrix <- matrix(1:16, 4, 4)
MatrixSol <- makeCacheMatrix(Matrix)
cacheSolve(MatrixSol)
