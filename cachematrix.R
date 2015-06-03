## These functions aid in creating the inverse of a large matrix. makeCacheMatrix
## create a list that contains information that be used to check if a matrix inverse is cached 
## and cacheSolve will check that list to see if there is a cached inverse. If so, it will retrieve it.
## If not, i will generate a new one.

## Creates a list that can be used to check if a matrix inverse is cached and retrieve it usign the
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  # initializing m
  m <- NULL
  set <- function(y) {
    x <<- y
    # nothing is cached yet
    m <<- NULL
  }
  # used in other function to retrieve the unaltered matrix
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # the list that provides variables needed for cacheSolve
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks if inverse already exists; if not, generates a new one

cacheSolve <- function(x, ...) {
  # retrieving m
  m <- x$getinv()
  # checking if m is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # calculating inverse
  m <- solve(data, ...)
  # altering the "setinv" entry in list from makeCacheMatrix
  x$setinv(m)
  m
}
