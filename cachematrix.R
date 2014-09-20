
## makeCacheMatrix and cacheSove are functions that calculate the 
## inverse of an invertible matrix and cache it for later use if
## needed.


## The function makeCacheMatrix takes an invertible square matrix 
## and creates a special 'matrix' object which consists of the matrix,
## a variable to cache the value of the inverse, and a list
## of functions to manipulate the matrix. The function does not do any
## errors checking.
## 
## @param     x an invertible square matrix. The default value for x is
##            NA
## @return    a list (set = set, get = get,getInverse = getInverse,
##                    setInverse = setInverse) of functions where:
##
##      set         : sets the value of the matrix
##      get         : returns the current value of the matrix
##      getInverse  : returns the current value of the inverse matrix
##      setInverse  : sets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## Holds the cached value
    inv_mat <- NULL
    
    ## A setter for the matrix
    set <- function(mat) {
        
        x <<- mat
        inv_mat <<- NULL
        
    }
    
    ## A getter for the matrix
    get <- function() x
    
    ## A setter for the inverse matrix
    setInverse <- function(inv) inv_mat <<- inv
    
    ## A getter for the inverse matrix
    getInverse <- function() inv_mat
    
    ## Return a list of availabe functions
    list(set = set, 
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)

}


## The function cacheSolve takes an object of the type returned by 
## makeCacheMatrix and  returns the inverse of the matrix. The value
## is already cache the function return the value in the cache. Otherwise,
## it calculate the inverse and cahce it. The function does not do any
## errors checking.
## 
## @param     x is of the type returned by makeCacheMatrix
## @return    the inverse of the matrix of x
cacheSolve <- function(x, ...) {
    
    ## If the inverse is already cached, 
    ## return it
    inv_mat <- x$getInverse()  
    if(!is.null(inv_mat)) {    
        message("getting cached inverse")    
        return(inv_mat)
    }
    
    ## Otherwise, compute the inverse, 
    ## cache it, and return the value
    mat <- x$get()
    inv_mat <- solve(mat, ...)
    x$setInverse(inv_mat)
    inv_mat
}

## Temporary for testing
testwith<-function(x= 4 * diag(3)){
    
    m <- makeCacheMatrix()
    inv <- cacheSolve(m)
    print(inv)
    
    m$set(x)
    print(m$get())
    
    inv <- cacheSolve(m)
    print(inv)
    
    cacheSolve(m)
    
}
