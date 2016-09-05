## The functiond contained in the ".R"file accept a matrix as input and return the inverse of the matrix supplied.
# The value returned however, is either
# (1)computed by cacheSolve() or
# (2)returned by cacheSolve() from the "Cached" instance of the variable, depending on whether the "cached" value is available.

# makeCacheMatrix() does the following:
# 1. Accepts a matrix via argument "x"
# 2. Contains the defintions of 4 functions, which will be passed to the cacheSolve function
# 3. Returns a list that contains functions definitions to "get" and "set" a matrix, and "get" and "set" its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL #set the inverse of the matrix to null each time a new matrix is passed.
  
  get <- function(){
    x
  }
  
  inverse <- function(x){
    inverse <- solve(x)
  }
  
  setInverse <- function(inverse){  #when this function is called, the inverse passed via tyhe fin is set to the global variasble inverse
    i <<- inverse            
  }
  
  getInverse <- function(){
    i
  }
list(get = get, inverse = inverse, getInverse = getInverse, setInverse = setInverse)  ##good version
}


# cacheSolve() does the following:
# 1. Accepts a list (passed via the return from the previous function).
# 2. Determines whether the inverse is already compouted and available. If yes, it returms the cac hed inverse
# 3. However if the cached ibverse id null, it indicates that the function "makeCacheMatrix()" was called and may have passed an updated matrix - 
#    therefore requiring the inverse to be re-computed.
# 4. If a "re-compute" is required, the re-compyed value is saved into the global value of "i" using the <<- operator, 
#    thus ensuring that subsequent calla to cacheSolve() have access to the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    print("Getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
