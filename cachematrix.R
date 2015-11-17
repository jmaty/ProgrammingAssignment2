## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly.

# The function makeCacheMatrix creates a special "matrix", which is really
# a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse of the matrix
# - get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # Init the inverse of the matrix
    i <- NULL
    
    # Define the set function to set the value of the matrix (and set its
    # inverse as "not solved yet")
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Define the get function to get the value of the matrix
    get <- function() x
    
    # Define setinv function to set the inverse of the matrix
    setinv <- function(inv) i <<- inv
    
    # Define getinv function to get the inverse of the matrix
    getinv <- function() i
    
    # Return the list of the functions defined above
    list(set=set,
         get=get,
         setinv=setinv,
         getinv=getinv)
}

# The function cacheSolve calculates the inverse of the special "matrix" created
# with the function makeCacheMatrix. The function first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from the cache
# and skips the computation. Otherwise, it calculates the inverse of the data
# and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    # Get inverse of the "special" matrix
    i <- x$getinv()
    
    # Check if the value of the inverse is cached (i.e. if it has been already
    # calculatd)
    if(!is.null(i)) {
        # The inverse is cached --> return it
        message("getting cached data")
        return(i)
    }
    
    # The inverse has not been cached --> get the data from the special "matrix"
    data <- x$get()
    
    # Calculate the inverse of the data by standard function solve()
    i <- solve(data, ...)
    
    # Set the value of the inverse (i.e. cache it) so that it needs not to be
    # calculated next time 
    x$setinv(i)
    
    # Return a matrix that is the inverse of 'x'
    i
}
