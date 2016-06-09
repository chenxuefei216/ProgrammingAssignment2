## The functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the above function.

cacheSolve <- function(x, ...) {	
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting the inverse matrix")
		return(m)
	}
	inverse <- x$get()
	m <- solve(inverse)
	x$setInverse(m)
	m
}


