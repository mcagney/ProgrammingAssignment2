## Megan Cagney - Programming Assignment 2
## October 23, 2014 (Happy Mole Day!)


## Cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## You must initialize the inverse property first
  ## by making i null
  i <- null 
  
  ## Next, you need to set the matrix as so, using the "<<-"
  ## in order to assign a value to  an object in an environment
  ## that is different from the current environment
  set.matrix <- function(matrix) {
    invmat <<- matrix
    i <<- null 
  }
  
  ## Now get the matrix and return it 
  get.matrix <- function(){invmat}
  
  ## Now set the inverse of the matrix
  set.Inverse <- function(inverse){
    i <<- inverse
  }
  
  ## Now get the inverse of the matrix and return it
  get.Inverse <- function() {i}
  
  ## Compile a list of methods -- return these
  list(set=set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
  


}


## This function computes the inverse of the special matrix 
## returned from the function (makeCacheMatrix above).
## If the inverse has already been calculated, then the cachesolve should
## retrieve the inverse from the cache
## instead of having to compute it again (which could take awhile)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Get the inverse matrix of x
  invmat <- x$getInverse()
  
  ## If the inverse has already been calculated, return the
  ## cached inverse 
  if(!is.null(invmat)){
    message("Getting Cached Inverse")
    return(invmat)
  }
  
  ##Get the matrix
  data <- x$get()
  
  ## Calculate the inverse
  invmat <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(invmat)
  
  ##Return the Matrix
  invmat
  
  
  
  
  
}
