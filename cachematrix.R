## Function to create matrix object that can cache its inverse
## V.Kopf; Oct. 2014; Coursera R Programming

## This function makes a matrix object which can maintain a cache of its inverse

makeCacheMatrix <- function(x=matrix())
{
     m <- NULL
     set <- function(y)
     {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix()
## If the inverse has been calculated, the inverse is returned from the cache

cacheSolve <- function(x, ...)
{
     m <-x$getinverse()
     if(!is.null(m))
     {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
