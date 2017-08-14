## Matrix Inversion is a process that can take a large amount 
## of computing power. These functions cache the inverse of 
## a matrix so that it does not need to be computed multiple
## times. 

## makeCacheMatrix does 4 things
# 1. Sets the value of a matrix
# 2. Gets the value of the matrix from step 1
# 3. Sets the value of the inverse of the matrix
# 4. Gets the value of the inverse of the matrix 

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


## cacheSolve returns the vatrix that is the inverse of the
## matrix x. Before calculating the inverse the function 
## checks to see if the solution is cached. If it has already
## been calculated it returns the cached answer. If not it 
## calculates the inverse and caches it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        
  
   ## Return a matrix that is the inverse of 'x'
}
