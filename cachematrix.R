## makeCacheMatrix stores (but doesn't calculate) the inverse of a matrix;
## cacheSolve does the inverse calculation

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)  inv<<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# calculates the inverse of the special matrix created by the makeCacheMatrix function above. 
# if the inverse has already been calculated, then it retrieves the inverse 
# from the cache

cacheSolve <- function(x, ...) {
    ## you will only get the message, if the inverse has been computed already
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    ## if mat is a square invertible, then solve(mat) returns its inverse
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

