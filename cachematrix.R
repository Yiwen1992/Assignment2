## caching the inverse of a matrix without computing it over and over again

## makeCacheMatrix creates a special "matrix", which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve calculates the inverse of the matrix created in the last function
## it first checks whether the inverse of the matrix has been calculated
## if so, it gets the inverse of the matrix from the cache and skips the computation
## Otherwise, it caculates the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix,...)
    x$setmatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
