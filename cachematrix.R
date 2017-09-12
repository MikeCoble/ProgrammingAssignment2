## Caching the Inverse of a Matrix

## Assumption: the supplied matrix is always invertible.

## This function creates a special "matrix" object that can cache
## its inverse.  It is really a list containing functions to:
##  1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the inverse of the matrix
##  4) get the inverse of the matrix

makeCacheMatrix <- function(mtx = matrix()) {
  
  # initialize the local variable 'inv' to be NULL to indicate
  # the inverse has not yet been calculated
  inv <- NULL
  
  # define the function 'set' that takes one argument, 'inmtx', a matrix
  set <- function(inmtx) {

    # give the variable mtx in the parent environment (i.e., the 
    # definition of makeCacheMatrix) the inputted matrix
    mtx <<- inmtx
    
    # reset the 'inv' variable in the parent environment (i.e.,
    # the definition of makeCacheMatrix) to indicate the inverse
    # (for the new matrix) has not been calculated
    inv <<- NULL
  }
  
  # define the function 'get' to return the matrix 
  #  note: since 'mtx' isn't defined within get(), retrieve it
  #  from the parent environment of makeCacheMatrix()
  get <- function() mtx
  
  # define the function 'setinv' to store the matrix inverse
  # in the 'inv' variable in the parent envrionment
  # (i.e., the definition of makeCacheMatrix)
  setinv <- function(ininv) inv <<- ininv
  
  # define the function 'getinv' to return the matrix inverse
  #  note: since 'inv' isn't defined within get(), retrieve it
  #  from the parent environment of makeCacheMatrix()
  getinv <- function() inv
  
  # store the 4 defined functions in a list and return
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

#  Takes one named argument, 'cmtx', which is a "CacheMatrix" made
#  matrix object.  Additional arguments are passed through to
# the solve() function that computes the inverse.

cacheSolve <- function(cmtx, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  # If the cache matrix object's inverse is not NULL
  cinv <- cmtx$getinv()
  if(!is.null(cinv)) {
    
    # then it is the correct inverse, return it
    message("getting cached data")
    return(cinv)
  }
  
  # Otherwise the cache matrix object's inverse needs to be 
  # calculated.  So get the cache matrix object's actual matrix
  mtx <- cmtx$get()
  
  # and send it to the solve() function, passing along any extra
  # arguments
  cinv <- solve(mtx, ...)
  
  # tell the cache matrix object about the new inverse -- to
  # be cached
  cmtx$setinv(cinv)
  
  # and return it
  cinv
}

# Function for testing the R functions 
# makeCacheMatrix and cacheSolve
# in the Coursera R Programming Course
#
testCacheMatrix <- function() {
  
#
# A simple matrix m1 with a simple matrix inverse n1
# Define
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > m1
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
 
# Use m1 to test your 
# makeCacheMatrix and cacheSolve functions.
#
# m1 was constructed 
# to have a simple marrix inverse, call it n1.
# This means  m1 %*% n1 (%*% is matrix multiply in R) 
# is the 2 row by 2 column Identity matrix I2
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
# > I2
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1

# And so (by linear algebra) n1 %*% m1 is also equal to I2.
# (If n1 is the inverse of m1 then m1 is the inverse of n1.)
# With m1 defined as above, n1 ( the inverse of m1) is
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
# > n1
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4

# Checks:
m1Xn1 <- m1 %*% n1
n1Xm1 <- n1 %*% m1

sm1 <- solve(m1)
sn1 <- solve(n1)

# So doing 
myMatrix_object <- makeCacheMatrix(m1)

# and then
myMatrix_inverse <- cacheSolve(myMatrix_object)

# should return exactly the matrix n1

# calling cacheSolve again should retrieve (not recalculate)
# n1
myMatrix_inverse <- cacheSolve(myMatrix_object)

# use the set function to "put in" a new matrix.
# For example n2
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

myMatrix_object <- myMatrix_object$set(n2)

# and obtain its matrix inverse by
myMatrix_inverse <- cacheSolve(myMatrix_object)

myMatrix_inverse <- cacheSolve(myMatrix_object)
}

testCacheMatrix2 <- function() {
  # test script for makeCacheMatrix.R
  #  based on script posted by Igreski in
  #  https://github.com/lgreski/datasciencectacontent/blob/master/resources/testMakeCacheMatrix.R
  
  # approach 1: create a matrix object, then use it as input to cacheSolve()
  
  a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
  cacheSolve(a)
  
  # call cacheSolve(a) a second time to trigger the "getting cached inverse" message
  cacheSolve(a)
  
  # multiply the matrix by inverse, resulting in identity matrix
  a$get() %*% a$getinv()
  
  # reset a with another matrix to clear out cached value
  a$set(matrix(c(2,3,2,2),2,2))
  
  # confirm that a has new data and that cache is NULL
  a$get()
  a$getinv()
  
  # rerun cache solve, note that "getting cached inverse" does not print,
  # and that we get a different result
  cacheSolve(a)
  
  # approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
  #             note that the argument to cacheSolve() is a different object
  #             than the argument to the first call of cacheSolve()
  cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))
  
  # try a non-invertible matrix
  b <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))
  cacheSolve(b)
  
  # illustrate getting the memory locations
  a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
  tracemem(a)
  tracemem(matrix(c(-1, -2, 1, 1), 2,2))
  
  # approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
  #             note that the argument to cacheSolve() is a different object
  #             than the argument to the first call of cacheSolve()
  cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))
  
  # illustrate getting the memory locations
  a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
  tracemem(a)
  tracemem(matrix(c(-1, -2, 1, 1), 2,2))
  
  # test non-matrix input: should return "not a matrix" error
  
  a$set(1:5)
  cacheSolve(a)
  
}