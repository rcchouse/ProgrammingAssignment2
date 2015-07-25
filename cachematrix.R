## This exercise is to write code to compute and cache a matrix inverse
## so it would not need to be computed repeatedly to be used repeatedly

#This will be done by modifying the sample code for storing the mean of a vector.

## The first part is to create a function-list to cache a matrix
## (i.e. create a list that containing functions to store and retrieve 
## the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of the special "matrix" created above.
## It first checks to see if the inverse is already stored in the object.
##     If so, it returns the stored value.
## Otherwise, it computes the inverse using "solve", stores the inverse in the 
## cache, and returns that inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
