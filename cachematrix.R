 # See README.md for instructions on running the code and output from it 
 # 
 # makeCacheMatrix is a function that returns a list of functions 
 # Its purpose is to store a martix and a cached value of the inverse of the  
 # matrix. 
 # Contains the following functions: 
 # * setMatrix      set the value of a matrix 
 # * getMatrix      get the value of a matrix 
 # * cacheInverse   get the cached value (inverse of the matrix) 
 # * getInverse     get the Inverse value (inverse of the matrix) 
 # 
 makeCacheMatrix <- function(x = numeric()) { 
          
         # Set cache to NULL 
         cache <- NULL 
          
         # Set matrix 
         setMatrix <- function(newValue) { 
                 x <<- newValue 
                 cache <<- NULL 
         } 
   
         # Return the matrix stored 
         getMatrix <- function() { 
                 x 
         } 
 
         # Cache the argument  
         cacheInverse <- function(solve) { 
                 cache <<- solve 
         } 
   
         # Get the cached value 
         getInverse <- function() { 
                 cache 
         } 
          
         # Return a list. Each named element of the list is a function 
         list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
 } 
 

# The following function calculates the inverse of a "special" matrix 
# created with makeCacheMatrix 
 
 cacheSolve <- function(y, ...) { 
         
         # Get the cached value 
         inverse <- y$getInverse() 
         
         # If cached value exists then return value 
         if(!is.null(inverse)) { 
                 message("getting cached data") 
                 return(inverse) 
         } 
         
         # Otherwise get the matrix, calculate the inverse 
         # and store it in the cache 
         
         data <- y$getMatrix() 
         inverse <- solve(data) 
         y$cacheInverse(inverse) 
          
         # Return the inverse 
         inverse 
 } 
