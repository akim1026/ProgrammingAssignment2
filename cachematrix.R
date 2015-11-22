## This code will cache the inverse of a matrix so it does not need to be computed each time for use.

## This function creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL                                       #initiate m to NULL
        set <- function(y){                     
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setinverse<-function(solve) m<<- solve
        getinverse<- function() m
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has been calculated it will return the value from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                #message("getting cached data")
                return(m)
        }
        data <-x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
