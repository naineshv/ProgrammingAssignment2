## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## matrixEqual: Compare matrices and return TRUE if equal, else return FALSE

## Example:
## m1 <- matrix(c(1:4),2)
## mc <- makeCacheMatrix(m1)
## cacheSolve(mc)
## cacheSolve(mc)        #from cache
## m2 <- matrix(c(5:8),2)
## mc[["set","matrix"]](m2)
## cacheSolve(mc)
## cacheSolve(mc)        #from cache
## --if the matrix hasnt changed, do not recalculate inverse
## mc[["set","matrix"]](m2)
## cacheSolve(mc)        #from cache

## makeCacheMatrix: This function creates a special "matrix" object which is a matrix of function objects.
## These functions get and set the matrix as well as its inverse.  The returned matrix object will look as below.
## The dimensions of the matrix have been named according to its purpose
## makeCachematrix(m1)
##    | matrix  inverse
## ---|-------------------
## set| set     setinverse
## get| get     getinverse
##    |

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                #update only if the matrix has changed
                if (!matrixEqual(x,y)) {
                        x <<- y 
                        i <<- NULL
                }
        }
        
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        m <- matrix(data =c(set, get, setinverse, getinverse), 2,2)
        dimnames(m)<-list(c("set","get"),c("matrix","inverse"))
        m
}

## cacheSolve: This function checks the cache to see if the matrix has an inverse already calculated 
## and returns the value if available.
## if not available, it calculates the inverse and returns it along with caching it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x[["get","inverse"]]()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x[["get","matrix"]]()
        i <- solve(data, ...)
        x[["set","inverse"]](i)
        i
}


## matrixEqual: Compare matrices and return TRUE if equal, else return FALSE
matrixEqual <- function(x, y) {
        (is.matrix(x) && is.matrix(y) 
        #&& dim(x) == dim(y) #if only the dims change, it does not affect the inverse, hence removing check
        && all(x == y))
}