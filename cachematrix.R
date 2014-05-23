## The following functions allow the user to compute the inverse of a matrix
## and cache the result for future use. This allows the user to request the inverse
## of the matrix repeatedly, and if the input matrix has not changed then
## the cached solution will be returned instead of recomputing the result.

## The makeCacheMatrix function accepts a square matrix as an input and returns 
## a list of functions for getting and setting the matrix, and computing and 
## returning the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    # this variable holds the solution, initially it is empty
    solvar <- NULL 
    # the set function will set the its input data as the data to use
    # for the matrix and will reset the solution to empty
    set <- function(y) {
        x <<- y
        solvar <<- NULL
    }
    # the get function will return the value of the data
    get <- function() x
    # the setinv function will allow the solution to be cached
    setinv <- function(solve) solvar <<- solve
    # the getinv function will return the value of the solution variable
    getinv <- function() solvar
    # return a list of the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function will return the inverse of the input matrix.
## If the inverse has not been computed, it will compute the inverse and cache the solution.
## If the inverse has already be computed, it will retrieve the cached solution.

cacheSolve <- function(x, ...) {
    # get the inverse data
    solvar <- x$getinv()
    # if the solution is not empty, get the cached solution,
    # return the solution and exit the function
    if(!is.null(solvar)) {
        message("getting cached data")
        return(solvar)
    }
    # if the solution is empty, get the matrix data
    data <- x$get()
    # solve for the inverse
    solvar <- solve(data, ...)
    # cache the inverse solution variable
    x$setinv(solvar)
    # return the solution variable
    solvar
}
