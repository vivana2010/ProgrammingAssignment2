## Below are a pair of functions that stores a matrix and caches its inverse.

## 1 This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Initiate the inverse property
        inv <- NULL
        ## Set the Matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        ## REturn a list of methods
                list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2 This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## Return the inverse if it is already set
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## get the matrix from the object
        mat <- x$get()
        
        ## Calc the inverse using matrix calc
        inv <- solve(mat, ...)
        
        ## set the inverse to the object
        x$setInverse(inv)
       
       ## Return the matrix
       inv
}
