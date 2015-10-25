## These functions are used to take in a square invertible matrix
## Create a special matrix object to store the matrix values
## and cache matrix inverse values if they have been calculated already


## Creates a matrix object that supplies a get, set for the value of the matrix
## As well as a getInverse and setInverse of the matrix
makeCacheMatrix <- function(x = matrix())
{
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    
    list(
        set = set, get = get, setInverse = setInverse, getInverse = getInverse
    )
}


## This takes in a special matrix object and returns the inverse
## of that matrix.  It will return the inverse without computing if object has
## inverse value stored, otherwise it will compute the inverse

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
