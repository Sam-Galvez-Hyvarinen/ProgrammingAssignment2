## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Define the makeCacheMatrix function
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL               ##This function creates a special "matrix" object that can cache its inverse
  set <- function(matrix) {  ##It initializes a variable inv to NULL
    mat <<- matrix
    inv <<- NULL            ## then defines the following 4 functions."set" "get" "setinv" and "getinv
  }
  get <- function() mat  ## retrieves value from the matrix
  setinv <- function(inverse) inv <<- inverse ## sets the value of the inverse matrix
  getinv <- function() inv    ## retreives the value for the inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()     ## This function retrieves the inverse from the cache using the "getinv()" function from my matrix
  if (!is.null(inv)) {    ## If the inv variable is not NULL, then this means that the inverse has already been calculated and cached
    message("getting cached inverse")    ## so the function will return the cached inverse matrix.
    return(inv)
  }
  data <- mat$get()        ##By using these two functions together, i am able to create a matrix  
  inv <- solve(data, ...)   ##I can then cache its inverse, and retrieve the cached inverse matrix for any future use
  mat$setinv(inv)         
  inv
}