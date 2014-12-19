## This package contains two functions:
##
## makeCacheMatrix() creates new objects(s) and contains 2 get and 1 set function.
##
## cacheSolve() accesses the object (not the makeCacheMatrix() function, but
## the object created when makeCaceMatrix was called). The inverse matrix is 
## then calculated if, and only if, the inverse has not already been calculated.

## makeCacheMatrix is passed a matrix which creates an "object" of type 'matrix'.
## This object stores two things, The original value of the matrix and what will
## be the cached value of its inverse, which is initially set to 'NULL'. Within
## the makeCacheMatrix() function there are also three functions, two to read 
## (or 'get') the values of the two things being stored, and one function to 
## change ('set') them.

makeCacheMatrix <- function(x = matrix()) {         # input will be a matrix
    
    m <- NULL        # m will be our inverse matrix and it is reset to NULL 
    # every time makeCacheMatrix() is called.
    
    # Note - these next three functions are defined but not run
    # when makeCacheMtrix is called.
    #
    # Instead, they will be used by cacheSolve() to get values
    # for x or for m (the inverse) and for setting the inverse.
    
    get <- function() { x }     # this function returns the value of the original 
    # matrix.
    
    setinv <- function(solve) { m <<- solve}
    # This is called by cacheSolve() during the first cacheSolve()
    # access and it will store the value using super-assignment.
    
    getinv <- function() { m }      # this will return the cached value of
    # cacheSolve() on subsequent accesses.
    
    list(get = get,                 # This is accessed each time makeCaceMatrix()
         setinv = setinv,            # is called, that is, each time a new object.
         getinv = getinv)            # This is the list of the internal functions
    # (methods) so a calling function knows how
    # to access these methods.
    
}


## cacheSolve() accesses the object (not the makeCacheMatrix() function, but the object
## created when makeCacheMatrix() was called) by fetching the matrix used to create the
## object, this matrix being stored when the object was created.
##
## If the inverse matrix has not been calculated (if it is still 'NULL') cacheSolve()
## calculates the inverse matrix, provided the determinant of the original matrix
## does not equal zero.
##
## If it does equal zero, the matrix does not have an inverse and an error occurs if 
## the solve() function is used. A message is printed to the console advising the
## user that the matrix cannot be solved and the screen returns to the '>' input prompt.
## 
## If the determinant does not equal zero then the inverse is calculated and cacheSolve()
## stores the result in the object created by the call to cacheMakeMatrix(), and then returns
## the inverse matrix.
##
## If the inverse matrix has already been calculated then cacheSolve() simply fetches it and
## returns the stored matrix, saving the computing time required to calculate the inverse
## matrix again.

cacheSolve <- function(x, ...) {            # the input x is an object created by makeCacheMatrix
    
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()                         # accesses the object 'x' and gets the value of the inverse
    if (!is.null(m)) {                      # if inv was aleady cached (not NULL) ...
        
        message("getting cached data")      # ... send this message to the console
        return(m)                           # ... and return the inverse matrix ... "return" ends
        
    }
    
    data <- x$get() # we reach this code only if x$getinv() returned NULL
    
    if (det(data) != 0) {                   # test that the determinant is not zero
        # if not, find the inverse
        m <- solve(data, ...)               # if m was NULL then we have to calculate the inverse matrix    
        
    }
    else {                                  # otherwise, print a message
        
        print("The determinant of the input matrix equals zero. Therefore")
        print(" the inverse cannot be calculated. Try entering another matrix")
        
    }
    x$setinv(m)                             # store the calculated inverse matrix in x (see setinv()) in makeCacheMatrix
    m                                       # return the inverse matrix to the code that called this function
}