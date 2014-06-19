## Put comments here that give an overall description of what your
## functions do

## This function creates a new object that will be used to cache a matrix inverse
## It returns a list of functions that can be used to:
##    1. set the matrix 
##    2. get the matrix 
##    3. set the matrix inverse
##    4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
        m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## This function returns the inverse of a matrix
## First argument is the matrix to invert
## Other arguments are passed to the solve function used to invert the matrix
## First this function tries to retrieve the matrix inverse from the cache.  
## If value is in the cache then this value is returned.
## If no value is found in the cache then the function computes the matrix inverse, 
## set it in the cache and returns the result

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}

