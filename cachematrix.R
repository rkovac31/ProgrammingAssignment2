## Caching the inverse of a matrix to reduce computation.
## The variables 'm' and 'x' are used to represent the 2 objects needed for this function.
## 'm' is the inverse matrix while 'x' is the initial matrix.

## The function 'makeCacheMatrix' makes a special matrix object and will cache the inverse 
## of the matrix where possible. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)	
}


## The function 'cacheSolve' will return the inverse matrix if that information is already cached. 
## If none is found it will compute the inversed matrix, using the special matrix object from the function 
## 'makeCacheMatrix.'

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if (!is.null(m)) {
		message ("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse (m)
	m
}
