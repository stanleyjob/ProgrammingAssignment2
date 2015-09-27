## Functio to perform Matrix inversion using caching to avoid costly recomputation
## Two functions are used, makeCacheMatrix (creates a matrix and provides access to it) and cacheSolve (which uses solve function to inverse the matrix )

## Creates special matrix object

makeCacheMatrix <- function(cache_inputmatrix = matrix()) {
  
  cache_minverse <- NULL             # Matrix inverse initially set to NULL
  
  setmatrix <- function(inputmatrix) {
    cache_inputmatrix <<- inputmatrix
    cache_minverse <<- NULL          # Whenever there is a new inputmatrix is assigned the cache_minverse is set to NULL (reinitialized)
  }
  
  getmatrix <- function() cache_inputmatrix       # returns the input matrix, which is not inversed
  
  setinverse <- function(minverse) cache_minverse <<- minverse          # sets the inverse of the matrix to the cache_minverse variable
  
  getinverse <- function() cache_minverse             # gets the current inverse value of the matrix from the cache varaible
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Calcualtes the inverse of the matrix using solve function

cacheSolve <- function(input_special_matrix, ...) {
  
        # Get inverse value
        m_inverse <- input_special_matrix$getinverse()
        
        # Check if inverse value has been calcualted, if calculated then retrun the calculted value
        # PLEASE NOTE if the value of the non-inverted matrix is changed by any means, the cached inverse matrix value will be 
        # set to NULL in the makeCacheMatrix  
        if(!is.null(m_inverse)) {
              message("Getting cached Inverse Matrix")
          
              return(m_inverse)
        }
  
        # If inverse is not calculated then 
        # Get the cached input matrix
        data_input_matrix <- input_special_matrix$getmatrix()
        
        m_inverse <- solve(data_input_matrix, ...)
      
        input_special_matrix$setinverse(m_inverse)
          
        ## Return a matrix that is the inverse of 'x'
        m_inverse
}

