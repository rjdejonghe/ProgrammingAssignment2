## The first function creates a matrix,  
## which is really a list containing several functions.
## The second function solves for the inverse of any matrix
## which is encoded using the special form given by the first function

## This next function creates a special "matrix" M, which is really a list containing
## the following four functions
## 1. set - takes a matrix A as an argument, and sets the value of M to be A  
## 2. get - returns M as an acutal matrix
## 3. setInverse - calculates the inverse of the matrix M, and saves it 
## 4. getInverse - retrieves the value of the inverse matrix of M, if it exists 

makeCacheMatrix <- function(x = matrix()) { ##set a defualt value of a blank matrix
      
      ## This variable will contain the Inverse matrix after it is computed 
      Inv <- NULL           
      
      ## This function can be used to set a new value of the matrix
      ## It automatically clears the Inverse matrix from memory
      set <- function(y)
      {
            x <<- y
            Inv <<- NULL
      }
      
      ## This function returns the matrix
      get <- function() x
      
      
      setInverse <- function(inverse) Inv <<- inverse
      getInverse <- function() Inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



## This functions takes a special matrix of the above form, and computes its inverse
## if that inverse is not already saved, otherwise it returns the saved value.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      Inv <- x$getInverse()
      if(!is.null(Inv)) {
            message("getting cached data")
            return(Inv)
      }
      data <- x$get()
      Inv <- solve(data, ...)
      x$setInverse(Inv)
      Inv
}
