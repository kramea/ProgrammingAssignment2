## These two functions can compute the inverse of the matrix,
## and can cache the inverted matrix, if it has to be computed repeatedly

## The function ‘makeCacheMatrix’ sets the value to the matrix, can retrieve it 
## through ‘get’ function, and finds the inverse of the matrix through ‘solve’ function.

## Also, the operator <<- is used to assign a value to an object in an environment 
## that is different from the current environment. This will be used for caching.

## This function computes the value, but does not return anything.


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


## The computed inverted matrix will be obtained and returned by this function.
## But, at first, the function checks if the inverse has already been computed before,
## and if yes, it skips the computation and returns the cached value with the message.

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
