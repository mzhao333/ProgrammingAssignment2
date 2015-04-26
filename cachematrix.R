##M.Zhao R Programming Course
##Programming Assignment #2



## Put comments here that give an overall description of what your
## functions do

##The Program is broken into two parts that ultimately produces an matrix and its
##inverse.



## Write a short comment describing this function:


##makeCacheMatrix does the following four functions, similar to the vector
##example. It will set the matrix, get the matrix, set the inverse, and get
##the inverse.


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <-function(y){
  ##Creating this subfunction to assign different values in a diff. environment
  ##from the original.
  x<<- y
  inv <<-NULL
  
}

get <- function() x
setinv <- function(inverse) inv <<-inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv=getinv)
  
}


## Write a short comment describing this function

##cacheSolve takes the special list of functions from the makeCachematrix
##function and makes the inverse of the original matrix. if the inverse has
##already been calculated, it will return the cache version.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    
    if(!is.null(inv)){
      #If inverse is already calculated, then returns the cached version
      message("Getting cached data")
      return(inv)  
    }
    ##If the inverse is not calculated, then retrieve other functions
    ##to solve for the inverse
    matdat <-x$get()
    inv <- solve(matdat,...)
    x$setinv(inv)
    
    return(inv)
    
}
