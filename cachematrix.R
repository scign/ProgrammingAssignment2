## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(newmatrix) {
		x <<- newmatrix
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## computes the inverse of matrix in makeCacheMatrix above
## If the inverse has already been calculated (and the matrix
## has not changed), then retrieve the cached inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
