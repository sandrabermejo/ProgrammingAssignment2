## Matrix inversion is usually a costly computation. 
## The next two functions are used to create a special object
## that stores a matrix and cache's its inverse,
## so when we need its inverse again, it can be looked up
## in the cache rather than computed repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## This object is really a list containing functions to
##		1. set the value of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse
##		4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL   # initialize the inverse matrix with NULL

		set <- function(y) {
			x <<- y	
			inv <<- NULL   # the inverse matrix needs to be reset to NULL
		}
		get <- function() x    
		setinv <- function(inverse) inv <<- inverse 
		getinv <- function() inv
		
		# Return the list with the functions created above
		list(set = set,
			 get = get, 
			 setinv = setinv,
			 getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, it gets the inverse from the cache.
## Else, it calculates the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		## If the inverse is not null, return cached data
		if(!is.null(inv)) {  
			message("getting cached data")
			return(inv)
		}
		## Inverse not previously computed
		matrix <- x$get()
		inv <- solve(matrix, ...)  ## compute inverse
		x$setinv(inv)   		 ## cache it
		inv            
}


## TEST

## myMatrix <- matrix(1:4, nrow = 2, ncol = 2)
## m <- makeCacheMatrix(myMatrix)
## cacheSolve(m)
## cacheSolve(m)