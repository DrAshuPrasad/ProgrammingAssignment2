## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# there are two functions:
# 1. makeCacheMatrix: provides for storing and retriving cached matrix.
# 2. cacheSolve: it gives inverse of a Matrix, and either gets it from cache,
#		or, calculates and stores in cache.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x

	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve function calculates inverse of a matrix;
# and, then stores the inverse in cache using makeCacheMatrix.
# on subsequent run, it retrives inverse from cache instead of calculating.
# caching results in avoiding repeated calculation of an expensive procedure.
# it uses set, get, setinverse, and getinverse to accomplish the same.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
