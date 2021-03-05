## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { # Assumed y as numeric
                x <<- y  # Assign value to the parent environment
                inv <<- NULL 
        }
        get <- function() x # Retrieve x from the parent environment
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, # Assign each of these functions as an element within the list
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dat <- x$get()
        inv <- solve(dat) # return the inverse of x, or calculate & return if cache is empty
        x$setInverse(inv)
        inv
}
