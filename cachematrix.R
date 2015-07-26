#The two functions evaluate the inverse of a given 
#invertible matrix by first checking in the cache 
#if the given matrix has been previously evaluated.

#The makeCacheMatrix function serves as a cache 
#for a previously evaluated matrix. It does so by 
#defining and returning a list of functions that 
#can store a matrix and its inverse, and can retrieve them as well.

makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#The cacheSolve function checks in makeCacheMatrix 
#if the given matrix has been previously evaluated. 
#If so, it retrieves the cached inverse and returns it. 
#If not, it solves for the inverse of the matrix and returns it.

cacheSolve <- function(x){
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
