## Put comments here that give an overall description of what your
## Write a short comment describing this function

## makeCacheMatrix takes a matrix as an argument and outputs a list of functions ## and results. including a cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
##create a blank matrix
inv <-NULL
set <- function(y) {
		x <<- y
		inv <<- NULL
}
		get <-function() x
		setinvmatrix <- function(solve)  inv <<- solve
		getinvmatrix <- function() inv
			list(set = set, 
				get=get,
				setinvmatrix=setinvmatrix,
				getinvmatrix=getinvmatrix
				)	

}

## Write a short comment describing this function
##checks for cache of solved inverse matrix,
##if it exists cached, it passes inverse gets returned, if it doesn't it
##creates the inverse matrix and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinvmatrix()
        if(!is.null(inv)) {
        		message("getting cached inverse matrix")
        		return(inv)
        }
		data <- x$get()
		inv <- solve(data,...)
		x$setinvmatrix(inv)
		inv
}
