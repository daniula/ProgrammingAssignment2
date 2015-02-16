## Put comments here that give an overall description of what your
## functions do

## Takes matrix and returns special matrix which is list containing functions:
## get() - returns initial matrix 
## set(y) - overwrites initial matrix with new one
## getsolved() - returns cached inverse of initial matrix
## setsolved() - caches inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    getsolved <- function() s
    setsolved <- function(solved) s <<- solved
    
    list(get = get,
         set = set,
         getsolved = getsolved,
         setsolved = setsolved)
}

## Takes special matrix and checks if it has cached inverse of matrix.
## If not it computes inverse of matrix and stores it in cache.
## It returns inverse of matrix as a result

cacheSolve <- function(x, ...) {
    solved <- x$getsolved()
    
    if (is.null(solved)) {
        matrix <- x$get()
        solved <- solve(matrix, ...)
        x$setsolved(solved)    
    }
    
    solved
}
