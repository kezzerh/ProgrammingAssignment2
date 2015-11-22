## The following function calculates the inverse of the special "matrix" created with the makeCachMatrix function.
## It first checks to see if the mean has already been calculated. If so, it gets the inverse from the cache and skips 
## the function call and thus computation. Otherwise, it calculates the mean of the data and sets the value of the inverse
## in the cache via the setInv function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
        
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x) {
    m <- x$getInv() 
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}
