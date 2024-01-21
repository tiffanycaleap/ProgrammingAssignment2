## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The goal of the makeCacheMatrix fxn below is to create a special "matrix" 
## object that can cache its inverse.
## ALL NOTES ARE SET OFF TO THE RIGHT TO KEEP WORKSPACE CLEAR. SCROLL -->>>>>
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                                                  ## inv is set as NULL in order to save the matrix inverse
        set <- function(y) {                                                         ## set fxn is what will provide new
                x <<- y                                                              ## matrix values within the parent environment
                inv <<- NULL                                                         ## for a new matrix, inv is NULL
        }
        get <- function() x                                                          ## get fxn returns matrix value
        
        setinverse <- function(inverse) inv <<- inverse                              ## assigns inv value to parent environment
        
        getinverse <- function() inv                                                 ## gets inv value
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## allows $ to function and call properly
}


## The cacheSolve fxn below computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.
## ALL NOTES ARE SET OFF TO THE RIGHT TO KEEP WORKSPACE CLEAR. SCROLL -->>>>>
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                                                        ## function to acquire the inverse
        if(!is.null(inv)) {                                                          ## if not is.null(inv) return calculated inv           
                message("getting cached data")
                return(inv)
        } 
        data <- x$get()                                                              ## calculations for inverse when necessary                                         
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
