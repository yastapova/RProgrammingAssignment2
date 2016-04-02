## Put comments here that give an overall description of what your
## functions do

## Creates a vector that contains functions to:
##      - set the value of the matrix
##      - get the value of the matrix
##      - set the value of the inverse
##      - get the value of the inverse

makeCacheMatrix <- function(x = matrix())
{
    # variable that holds the inverse
    i <- NULL
    # function that sets the matrix to something else
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    # function that returns the matrix
    get <- function() x
    
    # function that sets the inverse
    setinv <- function(inv) i <<- inv
    
    # function that returns the inverse
    getinv <- function() i
    
    # return the vector of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse of the matrix by first checking if
## it has already been calculated and cached.

cacheSolve <- function(x, ...)
{
    # gets the chached value of the inverse
    i <- x$getinv()
    
    # if it's not null, returns it
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    
    # otherwise, have to calculate the inverse
    # first get the matrix
    data <- x$get()
    
    # calculate the inverse
    i <- solve(data, ...)
    
    # cache the inverse
    x$setinv(i)
    
    # return the inverse
    i
}
