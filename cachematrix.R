## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setm <- function(mi) m <<- mi
        getm <- function() m
        list(set = set, get = get,
             setm = setm,
             getm = getm)
}

## The second function to use "m" to calculate matrix inversion and cache the result.
CacheSolve <- function(x,...){
        m <- x$getm()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setm(m)
        m
        
}

m <- matrix(sample(1000,9),3,3)
m1 <- makeCacheMatrix(m)
m2 <- cacheSolve(m1)