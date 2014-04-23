## The following two functions are used to create a special object that
## stores a numeric matrix and caches its inverse


## makeCacheMatrix() contains four different function definitions 
## that get and set a matrix values, compute the inverse of the matrix 
## and return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve returns the inverse of 'x'
## The function first calls the getinverse() function 
## in the makeCacheMatrix() function above.  If the inverse
## has already been computed and the matrix has not changed since
## the cached inverse is rerturned.  If not, the inverse is computed and 
## returned and in addition, the inverse is cached using the setinverse() function defined in
## makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
