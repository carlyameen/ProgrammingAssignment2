## These two functions will first, create a matrix object that can cache it's inverse,
## and then compute the inverse of that matrix object, retrieving the cached inverse
## matrix if it has already been calculated (and not changed).

## First Function - "makeCachematrix"
## to cache the inverse of a matrix object.
makeCachematrix <- function(x=matrix()){      # defining our matrix x()
  m=NULL
  set <- function (y) {
    x<<-y
    m<<-NULL
  }
  get <- function () x
  setinverse<- function (solve) m <<-solve #here we use the function solve() to get the inverse of matrix, and call it m
  list (set = set, get = get,               
        setinverse = setinverse,
        getinverse = getinverse)
}

## Second Function- "cacheSolve"
## to compute the inverse of the matrix object
## or to retrieve it's value if already computed (and not changed)
cacheSolve <- function (x, ...){
  m <-x$getinverse()
  if(!is.null(m)){
      message ("getting cached data") # here we look for a previously cached value for the inverse of matrix
      return (m)    # if there is a cached value, we return it here.
  }
  data <- x$get()
  m <-solve(data, ...)  #here we compute the inverse of matrix if the value has not been previously cached
  x$setinverse(m)
  m
}


