## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## param matrix to be created (represented by symbol x)
makeCacheMatrix <- function(x = matrix()) {

    ## initialize the cached solved inverse matrix value (symbol is m) as NULL
    m <- NULL
    
    ## getter function - returns the passed in matrix (symbol is x)
    get <- function() {
        x
    }
    
    ## setter function
    set <- function(y) {
        ## set the 'internal matrix' (symbol is x) from the local param passed (symbol is y)
        x <<- y
        ## because the matrix has been set to a new value (the cached solved matrix needs to be reset)
        m <<- NULL
    }
    
    ## getter function for the cached solved inversed matrix
    getsolve <- function() {
        m
    }
    
    ## setter function for the cached solved inversed matrix
    setsolve <- function(solve) {
        ## set the cached solved matrix (symbol is m) to be the passed in param matrix (symbol is solve)
        m <<- solve
    }
    
    ## return a list of four variables, each of which represents the getter and setter functions
    list(set = set, get=get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## get the cached solved matrix from the param x and store as m
    m <- x$getsolve()
    
    ## check if m is not null (previously calculated, then return it)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## m is null and needs to be calculated
    else { 
        ## retrieve the matrix and store as data 
        data <- x$get()
        
        ## Computing the inverse of a square matrix can be done with the `solve`
        ## function in R. For example, if `X` is a square invertible matrix, then
        ## `solve(X)` returns its inverse.
        
        ## use solve function to solve for the inverse matrix and store as m
        m <-solve(data, ...)
        
        ## use setter function to set the param x solved matrix as m
        x$setsolve(m)
        
        ## Return a matrix that is the inverse of 'x'
        m 
    }
}


