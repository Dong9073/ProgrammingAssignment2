## Caching the Inverse of a Matrix

## Create a special "matrix" which can get or set value of the matrix or its inverse
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() {
		return(x)
	}
	setsolve <- function(solve) {
		s <<- solve
	}
	getsolve <- function() {
		return(s)
	}
	list(	set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
	} else {
		data <- x$get()
		s <- solve(data, ...)
		x$setsolve(s)
	}
	return(s)
}
