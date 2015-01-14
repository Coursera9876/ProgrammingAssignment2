#
# Implementation of Programming Assignment 2 of Coursera's course "R Programming"
#
# makeCacheMatrix and cacheSolve are functions for computing and caching
# inverse matrices.
#

#
# Matrix cache which provides the following functionality:
# - store/retrieve a matrix
# - store/retrieve the inverse matrix
#
makeCacheMatrix <- function(x = matrix()) 
{
	invMatrix <- NULL  # we don't yet know the inverse matrix
	
	# Set the matrix which implicitely resets the cached inverse matrix
	set <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}
	
	# Returns the matrix
	get <- function() {
	        x
	}
	
	# Stores the inverse matrix
	setInvMatrix <- function(inv) {
	        invMatrix <<- inv
	}
	
	# Returns the inverse matrix (or NULL if no inverse matrix is stored yet)
	getInvMatrix <- function() {
	        invMatrix
	}
	
	# Return a list with the set and get methods
	list(set = set, 
		 get = get, 
		 setInvMatrix = setInvMatrix, 
		 getInvMatrix = getInvMatrix)
}

#
# Compute the inverse matrix using the matrix cache
# First it will be checked if there is already the inverse matrix available
# in the cache. In that case this matrix can be returned immediately.
# Otherwise the inverse matrix will be computed and stored in the cache
#
cacheSolve <- function(x, ...) 
{
        # Test if the result is already cached
	invMatrix <- x$getInvMatrix()
	if (!is.null(invMatrix)) {
		message("Getting cached data")
		return(invMatrix)
	}
	
	data <- x$get()           # retrieve the original matrix
	invMatrix <- solve(data)  # compute the inverse matrix
	x$setInvMatrix(invMatrix) # store the result in the cache
	
	invMatrix                 # return the inverse matrix
}

#
# Testing
#
#x = matrix(c(2, 4, 3, 1, 5, 7, 1/2, 1/3, 1/4), nrow=3, ncol=3)
#m = makeCacheMatrix(x)
#cacheSolve(m)
#cacheSolve(m)
