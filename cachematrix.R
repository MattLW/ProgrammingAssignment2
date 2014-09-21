## Put comments here that give an overall description of what your
## functions do

## NOTE: CODING TEMPLATE PROVIDED BY THE EXAMPLE IN THE ASSIGNMENT DESCRIPTION.

##When makeCacheMatrix called, creates a matrix and returns list containing a reference to each function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #This line sets variable 'i' (invertedmatrix) to NULL
  
  ##The "set" function sets 'x' to the argument 'y' and sets 'i' to NULL. 
  ##This child function can change the value of 'x', without calling the parent function
  ##and also nulls out any stored inverted matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##The "get" function returns the value of 'x'.
  get <- function() x
  
  ##The "setinverse" function sets 'i' in makeCacheMatrix to 'inverse'.
  ##Not intended to be used by itself (requires cacheSolve to function as intended).
  setinverse <- function(inverse) i <<- inverse
  
  ##"getinverse" function returns the value of 'i'.
  getinverse <- function() i
  
  ##"makeCacheMatrix" now returns labeled vectors of child functions:
  ##set; get; setinverse; getinverse.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##When cacheSolve is called, function will return a matrix that is the inverse of the 'x' matrix
##provided by "makeCacheMatrix".

cacheSolve <- function(x, ...) {

  i <- x$getinverse() #This line assigns the result from getinverse to 'i'.
  
  ##If statement to check assignment of 'i'.
  if(!is.null(i)) {
    message("getting cached data") #If 'i' not 'NULL' returns message, and the inverted matrix.
    return(i)
  }
  
  data <- x$get() #Since 'i' is 'NULL', sets data to 'x' from makeCacheMatrix.
  i <- solve(data, ...) #This line calculates 'inverse' of data (matrix), and assigns to 'i'.
  x$setinverse(i) #Refer to "setinverse" function in makeCacheMatrix.
  i #Returns the inverse.
}

## NOTE: CODING TEMPLATE PROVIDED BY THE EXAMPLE IN THE ASSIGNMENT DESCRIPTION.
