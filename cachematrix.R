## makeCacheMatrix returns a list of functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  # inv stores the cached inverse matrix
  inv <- NULL
  
  # setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter for the matrix
  get <- function() x
  
  # setter for the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # getter for the inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve calculates the inverse
## if the inverse has already been calculated, it returns the stored inverse


cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  
  # if the inverse is already calculated, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse is not yet calculated, calculate it
  data <- x$get()
  inv <- solve(data, ...)

  # cache the inverse
  x$setinverse(inv)
  
  # print the inverse
  inv
}
