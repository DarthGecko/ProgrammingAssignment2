## Caching data to prevent unneeded CPU usage
## In this case, caching the inverse of a matrix x.

## Produce a matrix which can store it's own inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { #make the matrix something else
                x <<- y
                inv <<- NULL #erases the old inverse
        }
        get <- function() x                     #return the matrix with the $get() command
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, #create a list with all these variables, making them accessible 
             setinverse = setinverse,
             getinverse = getinverse)
}


## With an input of a makeCahceMatrix list, goes about checking
#if an inverse is stored or solves for one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) { #Is there a value for the inverse cached? If so...
                message("getting cached data") 
                return(inv) #and thus ends execution.
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
