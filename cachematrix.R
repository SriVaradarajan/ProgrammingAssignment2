## The idea behind this exercise is to simulate "object-oriented" behaviour in R and to show the lexical scoping
## of global operators
## The function makeCacheMatrix behaves like a "Class" with data members that can be accessed
## through member functions ( functions inside functions in R) ( get, set, getInverse, setInverse)
## The key concept here is the <<- operator
## It is a "super assignment" operator, which means that the assignment is done in the global environment,
##  rather than to a variable that is within a function and therefore may only have local scope. 
## This allows class data members to hold their values even as we exit the functions
## where the their values are set

## Functional purpose: Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly
## The caching is done in a makeCacheMatrix "object" !
 
## function makeCacheMatrix is a "class" that exposes set, get, setInverse, getInverse methods for 
## a matrix. Once set and setInverse are called, the values of the matrix and it's inverse are stored 
## globally and acts as a cache
## Input can be a matrix or () as shown below
##  x <- matrix(3:6, 2, 2)
##  M <- makeCacheMatrix(x) 
##  M$get()
## OR 
## M <- makeCacheMatrix()
## Return value is an object (!) with a list of set and get functions that can be called from a calling function
## like CacheSolve

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize Inverse to NULL

        INV <- NULL

        ## Set the matrix value and initialize the inverse to NULL
	## example call M$set(matrix(2:5, 2, 2))

        set <- function(y){
		
		## set x to input argument, note use of <<- operator
                x <<- y
                ## Note: INV is set to  NULL everytime we call set()
                INV <<- NULL
               
        }
	
	## get simply returns the value of the matrix
	## example call M$get()

        get <- function() x
       
        ## setInverse sets the value of INV to the computed inverse
	## example a call M$setInverse(NULL)

        setInverse <- function(solve) INV <<- solve
        
	
	## getInverse simply returns the Inverse
	## example call M$getInverse()

        getInverse <- function() INV
      
        ## creates a list of the functions that are returned when makeCacheMatrix is called

        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
       
}

##
## The cacheSolve is a calling function that simply checks to see if a 
## a given makeCacheMatrix() "object" contains an Inverse value
## If it there the Inverse is used else it is computed
##

cacheSolve <- function(x, ...) {

	## Return inverse from cache, could be a value if present 
	## or NULL if not present
        INV <- x$getInverse()
        
        
	## If already present in cache, return cached value of inverse
        if(!is.null(INV)){
                message("return value from cache")
                return(INV)
                     
        }
        ## Else If Inverse = NULL (not in cache) , then compute Inverse
        message("newly calculating data")
        data <- x$get()
        INV <- solve(data, ...)

	## Sets Inverse to the newly calculated value   
        x$setInverse(INV)
  
	##Returns the new Inverse value
        INV
}

