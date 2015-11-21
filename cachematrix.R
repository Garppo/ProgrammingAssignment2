## These two functions are used to create a special matrix object, compute it's inverse,
## and then cache the inverse to be recalled later with being re-computed.

## makeCacheMatrix creates a special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL                                    ##m_inv will be the matrix inverse
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m_inv <<- solve
  getsolve <- function() m_inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## casheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed, then it
## will retrieve the inverse from the cashe.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getsolve()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m_inv)
  m_inv
}
