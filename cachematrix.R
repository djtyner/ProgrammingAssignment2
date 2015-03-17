## This file contains two functions that "work together" to compute
##  and store the store the value of the inverse or a matrix.  Once
##  the inverse of a particular matrix has been computed, the result
##  is cached.  If the matrix has not changed on consecutive function
##  calls, then rather than re-computing the inverse, the inverse 
##  matrix will be retrieved a the cached value.  If the matrix
##  has changed, however, the new inverse will be computed and the
##  result will be cached.




## The makeCacheMatrix function accepts a matrix as its input
##  argument and stores the matrix in 'x'.  It then defines four 
##  functions that will be used to create an object which is a 
##  list of the four functions.  Those four functions are
##    1) set - this function sets the value of a matrix by storing
##             it in 'x'
##    2) get - this function gets the value of the matrix that is 
##             stored in 'x'
##    3) set_inverse - this function computes the value of the 
##                    matrix inverse and stores it in 'inv'
##    4) get_inverse - this function gets the value of the matrix
##                    inverse that is stored in 'inv'.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize 'inv' to NULL because no inverse has been computed
        inv <- NULL
       
        ## Definition for the 'set' function.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
       
        ## Definition of the 'get' function.
        get <- function() {
                x
        }
       
        ## Definition for the 'set_inverse' function.
        set_inverse <- function(inverse) {
                inv <<- inverse
        }
       
        ## Definition for the 'get_inverse' function.
        get_inverse <- function() {
                inv
        }
     
        ## Create the output of the makeCacheMatrix function, which
        ##  is a list o the four function defined inside of
        ##  makeCacheMatrix.
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}
 
   
   
   
## The cacheSolve function accepts a list as it's input.  It will
##  check to see if the inverse of a matrix has been computed and
##  cached. If so, it will return the cached inverse.  If not, it
##  will compute the inverse and cache it.
cacheSolve <- function(x, ...) {
         ## Get the value in cache for the inverse matrix.
         inv <- x$get_inverse()
         
         ## If the cached value is not NULL, get the cached inverse.
         if(!is.null(inv)) {
                 message("Retrieved the cached inverse matrix.")
                 return(inv)
         }
         
         ## Is the cached value is NULL, get the "new" matrix, compute  
         ##  its inverse, and cache the inverse matrix.
         data <- x$get()
         inv <- solve(data)
         x$set_inverse(inv)
         inv
}
