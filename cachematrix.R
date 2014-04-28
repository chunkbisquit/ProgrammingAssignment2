## these functions together can solve for the inverse of a matrix
## and store the result. The "set" function in makeCacheMatrix
## doesn't ever get used but is included for symmetry

## this function takes a matrix as its argument and creates and returns
## a list of functions to get or set the matrix and to get or set
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function takes a prepopulated makeCacheMatrix object as its argument
## and checks for a cached inverse with getinverse. If none exists it solves
## for the dataset's inverse and stores it with setinverse

cacheSolve <- function(x, ...) {
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
