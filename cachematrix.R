## makeCacheMatrix = create special matrix where x is the matrix passed as an arg, inv is its inverse
## cacheSolve = retrieve solution from cache or calculate and cache if DNE where x is the special matrix passed as an arg

## create a cache matrix that stores its inverse in a list(with get/set functions)
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setsolve <- function(solve) inv <<- solve
	getsolve <- function(){
		inv
	}
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}


## function for retrieving matrix inverse(calculate and cache if DNE)
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getsolve()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	message("retrieving matrix")
	data <- x$get()
	message("calculating inverse")
	inv <- solve(data, ...)
	message("caching inverse")
	x$setsolve(inv)
	inv
}
