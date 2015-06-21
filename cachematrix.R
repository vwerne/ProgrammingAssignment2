## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    ## sets the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ## gets the value of the matrix
    get <- function() x
    ## saves the value of the solve
    setSolve <- function(solve) s <<- solve
    ## gets the value of the inverse
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## gets the inversed matrix from a special object created by makeCacheMatrix 
cacheSolve <- function(x, ...) {
    ## if it is available gets cached matrix saved in x and returns its inverse saved as s 
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## otherwise computes the matrix from x, saves it to the cache and returns s
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}