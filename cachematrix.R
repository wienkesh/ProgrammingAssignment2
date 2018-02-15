## This project contains a pair of functions that can cache the inverse of a matrix so that it it has already
## been calculated, a subsequent call to cacheSolve for the same matrix will not need to run the inverse function again,
## thus saving computation time.

## This function creates a special matrix object that can cache its inverse. It populates 2 matrix objects in the 
## function environment, the inital matrix x and the inverted matrix m
## It returns a list of 4 functions that:
## sets (or resets) the value of a matrix (set) in the parent environment, 
## gets the value of a matrix (get) in the parent environment, 
## sets the value of a matrix in the parent env to the original matrix in the parent env (setinv), 
## gets the value of the inverted matrix in the parent env (getinv)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function takes a list of functions returned by makeCacheMatrix (along with that entire funtion environment) 
## and returns the inverse matrix object from that environment
## If the inverse has been previously cached then it returns the cached matrix, otherwise
## it solves the matrix to generate the inverse and sets that in the cache 
## This function operates on the special matrix (m) that was cached in the makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()                             ## tries to get the cached inverse
    if(!is.null(m)) {                           ## if its populated then return it with a message
        message("getting cached data")
        return(m)
    }
    data <- x$get()                             ## otherwise get the value of x (original matrix)
    m <- solve(data, ...)                       ## generate its inverse and store it in a local variable m
    x$setinv(m)                                 ## call the function to reset the cache
    m                                           ## return the local m matrix
    
}
