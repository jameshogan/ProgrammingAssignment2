## James Aaron Hogan
## Programming Assignment 2
## Coursera - Data Science Specialization
## 2/27/2014



##  the "makeCacheMatrix" functions creates a special matrix object, and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead  find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    inv_x<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setinverse = setsolve,
       getinverse = getsolve)
}
}


## The cacheSolve functions returns the inverse of a matrix A created with  the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it; otherwise, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
