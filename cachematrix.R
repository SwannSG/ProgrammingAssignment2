## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() creates makeCacheMatrix object (x) with set, get,
##   setInverse, getInverse methods
## cacheSolve(x) updates x.setInverse(inverseMatrix) and caches value. 


## Write a short comment describing this function
## makeCacheMatrix has methods
##  set(someMatrix)
##  get() returns someMatrix
##  setInverse(inverseMatrix)
##  getInverse() returns inverseMatrix
## Usage:
## instance = makeCacheMatrix()                 / instantiate makeCacheMatric
## instance$set(someMatrix)                     / pass someMatrix to the instance
## instance$get()                               / get matrix of the instance someMatrix
## instance$setInverse(inverseMatrix)           / set inverse of someMatrix
## instance$getInverse()                        / get inverse of someMatrix
makeCacheMatrix <- function(x = matrix()) {
  # im, inverese matrix
  im <- NULL
  set <- function(someMatrix) { 
    # explicitly assign x and inverseMatrix to parent scope
    x <<- someMatrix
    im <<- NULL
  }
  # return matrix x
  get <- function() x
  setInverse <- function(inverseMatrix) im <<- inverseMatrix 
  getInverse <- function() im
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function
## cacheSolve expects parameter x which is a memCacheMatrix object
##  if x.getInverse() exists, do nothing as value already cached
##  if x.getInverse() does not exist, compute the inverse matrix
##  and update x.setInverse() to inverse matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if (!is.null(im)) {
    message("inverse matrix cached")
    return (im)
  }
  ## get the matrix
  m <- x$get()
  ## compute the inverse, we need to check if inverse exists
  if ( tryCatch(solve(m), error = function(e) "error") != "error") {
    ## inverse matrix can be solved
    im <- solve(m)
    x$setInverse(im)
  }
  else { 
    ## inverse matrix cannot be solved
    return ("No inverse matrix")
  }
} 

## test data
## matrix with inverse
#print('matrix with inverse')
#m1 = matrix(c(1,1,4,3,1,4,4,4,0), nrow=3, ncol=3)
#mco1 <- makeCacheMatrix()
#mco1$set(m1)
## cache the inverse matrix first time 
#print (cacheSolve(mco1))
## get value from cache
#print (cacheSolve(mco1))
## get value from cache
#print (cacheSolve(mco1))

# matrix with no inverse
#print("matrix with no inverse")
#m2 = matrix(c(3,5,1,6,2,2,3,1,1), nrow=3, ncol=3)
#mco2 <- makeCacheMatrix()
#mco2$set(m2)
#print (cacheSolve(mco2))

