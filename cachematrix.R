## The makeCacheMatrix and cacheSolve function allow to catch the inverse of 
## a matrix once and storing it to eliminate the need to calculate it again after

## The makeCacheMatrix creates a special object which is a list of four functions:
## 1) set the value of the matrix (set) which allows you to change the matrix value
## and then sets the inverse to NULL
## 2) get the value of the matrix (get) to see which matrix is stored
## 3) set the value of the inverse of the matrix to the given input (setinv)
## 4) get the value of the inverse of the matrix (getinv) that is stored

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The cacheSolve function calculates the inverse of the special construction
## from makeCacheMatrix function. It first looks if the inverse matrix is stored
## already and if so, returnes a message and the cached data.
## If no value for the inverse is stored, it calculates the inverse with the 
## solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
    
}
