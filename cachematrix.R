## In this assignment we are trying to implement functions to cache the
## operations (calculating inverse of a matrix) which are very costly and 
## calculated repeatedly, so that next time if the matrix is same then the
## is returned directly from the cache.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inv <<- inv
	getinv <- function() inv
	list(set = set,  get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse using
##solve() function of the data and sets the value of the inverse in the cache
##via the `setinv` function.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv))
	{
		message("getting cached inverse")
		return(inv) 
	}	
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv 	
	
}
