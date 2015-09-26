# Below are two functions that are used to create a special 
# matrix that stores a matrix and cache's its inverse.


##This function creates a special "matrix" object that can cache its inverse
#The function will create and return functions that can:
#set the matrix, get the matrix, set and get the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL  
  #function to set matrix
  set <- function(y){  
    x <<- y    
    i <<- NULL #store matrix in cache   
  }  
  get <- function() x #get matrix  
  setInverse <- function(solve) i<<- solve #set inverse matrix  
  getInverse <- function() i #get inverse matrix  
  list(set = set, get = get,  
       setInverse = setInverse,  
       getInverse = getInverse)  ## create list of functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# cacheSolve take a custom matrix type created by the makeCacheMatrix function  
# and calculates the inverse matrix of it  
# but first it checks to see if the calculation has been done before  
# if it has been done before it recalls the data from the cache. If it has not been done   
# before it calculates the inverse matrix then store it in the cache  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()                 #query the x matrix's cache  
  if(!is.null(i)){                    #if there is a cache the inverse has been previously calculated  
    message("getting cached data")    # sent message indicating this is just cache   
    return(i)                         # return the cache    
  }  
  data <- x$get()                     # get the matrix used by makeCacheMatrix function   
  i <- solve(data, ...)               # calculate the inverse of the matrix  
  x$setInverse(i)                     # store the inverse matrix in cache using the makeCacheMatrix set function  
  i                                   # return the inverse
}
