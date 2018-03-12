
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix

cacheSolve


makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
      inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
      set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
      }
     get <- function() x                     ## define the get fucntion - returns value of the matrix argument
     setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
     getinverse <- function() inv                     ## gets the value of inv where called
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)  ## you need this in order to refer 
                                    ## to the functions with the $ operator
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
}