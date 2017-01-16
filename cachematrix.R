## Functions claculate inverse matrix first and then store it 
## in cache for further use.  

## Set and get matrix and inverse matrix
 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- matrix()
    set <- function(y) {
        x <<- y
        inv_x <<- matrix()
    }
    get <- function() x
    setinverse <- function(solve) inv_x <<- solve
    getinverse <- function() inv_x
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    }
 
## Checks whether there is an inverse matrix stored in cache. 
## If no, calculate and store. If yes, use stored value.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if(!all(is.na(inv_x))) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$setinverse(inv_x)
    inv_x
}