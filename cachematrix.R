## the programming assignment for week3 of the course R programming 
## by Xue Tan

## Assignment: Caching the inverse of a Matrix

## Fist function: to create a special "matrix" object that can cache its inverse
## It contains following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Second function: to calculate the inverse of the special "matrix" of first function
## the steps are:
## 1. check if the inverse has already been calculated
## 2. if so, get the inverse from the cache
## 3. if not, calculate the inverse and set the value of inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
