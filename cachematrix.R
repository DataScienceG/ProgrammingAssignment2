## DataScienceG
## Put comments here that give an overall description of what your
## functions do 
## Examples to call this function:
## cacheSolve(makeCacheMatrix(matrix(c(2,4,3,1,5,7,4,5,8),nrow=3,ncol=3,byrow=TRUE)))
## cacheSolve(makeCacheMatrix(matrix(c(2,4,3,1),nrow=2,ncol=2,byrow=TRUE)))
## Example to solve if Det=0, there is no inverse matrix
## cacheSolve(makeCacheMatrix(matrix(c(3,6,3,5,2,1,1,2,1),nrow=3,ncol=3,byrow=TRUE)))
## 
## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## First check if Determinant is not 0
  detInv<-det(data)
  if (detInv==0){message("Determinant is zero, no inverse matrix")
                
  }
  ## if not then find inverse matrix
  else
  {
    inv <- solve(data,)
    x$setinverse(inv)
    return(inv)
  }
  
}


  
