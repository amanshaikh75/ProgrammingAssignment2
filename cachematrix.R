## The functions allow user to compute inverse of a matrix.
## When the inverse of a matrix is computed first time, it
## can be saved to eliminate need of computing the inverse
## repeatedly.
##
## The way to use the functions is:
##	cached_m <- makeCacheMatrix(m)
##	inv <- cacheSolve(cached_m)
##      .... some time later ....
##    inv <- cacheSolve(cached_m)

## Given a matrix, this function creates an object
## for storing its inverse when needed.
## Note: this object does NOT actually compute inverse;
## it allows user to merely store the inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL	# Inverse of 'x'.
	
	# Sets 'x' to the (newly) specified matrix 'y'.
	# Re-initializes 'inv_x' to NULL.
	set <- function(y) {
		x <<- y
		inv_x <<- NULL
	}
	
	# Returns the matrix stored.
	get <- function() {
		return(x)
	}

	# Sets inverse of the matrix to specified 'inv'.
	set_inv <- function(inv) {
		inv_x <<- inv 
	}

	# Returns the inverse matrix stored.
	get_inv <- function() {
		return(inv_x)
	}

	# Return the above defined functions as a list.
	return(list(set = set, get = get,
                  set_inv = set_inv,
                  get_inv = get_inv))
}

## Given a matrix, the function returns its inverse -- either
## by computing it or from a cache.
## The function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
	inv <- x$get_inv()
	if (!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}
	m <- x$get()
	inv <- solve(m)
	x$set_inv(inv)
}