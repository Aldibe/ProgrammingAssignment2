## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # create an attribute to keep the inversed matrix data
  i <- NULL
  
  # function to get and set cache matrix data
  set <- function(y) {
    x <<- y
    i <<- solve(y)
  }
  get <- function() x
  
  # function to get and set inversed cache matrix data
  setinverse <- function(y) i <<- y
  getinverse <- function() i
  
  # putting the function to the object
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # grabbing inversed matrix data from cache
  i <- x$getinverse()
  
  # if there's already an inversed matrix data, then
  # function will return that matrix
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if not, then function will make inversed matrix and
  # assign it to the cached matrix data
  d <- x$get()
  i <- solve(d)
  x$setinverse(i)
  
  # return the inversed matrix
  i
}
