## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

## First creates matrix by matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
## set the value of the Matrix eg. matrix$set(matrix(c(1, 2, 3, 4), 2, 2))
## get the value of the Matrix eg. matrix$get()
## set the value of the Inverse Matrix eg. matrix$setinverse(matrix(c(1, 2, 3, 4), 2, 2))
## get the value of the Inverse Matrix eg. matrix$getinverse()


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.
## eg. cacheSolve(matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinverse()
       if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
