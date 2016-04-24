makeCachematrix <- function(x) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinv <- function(solve) k <<- solve
  getinv <- function() k
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

b<- makeCachematrix(matrix(c(1,2,3,4),nrow=2,ncol = 2))

b$get()

cacheSolve <- function(x, ...) {
  k <- x$getinv()
  if(!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data)
  x$setinv(k)
  k
}

cacheSolve(b)

