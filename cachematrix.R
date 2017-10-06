## Put comments here that give an overall description of what your
## functions do

## This function provides the means to calculate inverse of the given matrix and stores the matrix and it's inverse for later use


makeCacheMatrix <- function(x = matrix()) {
	invX <- matrix()                     ## Sets inverse to an empty matrix
	set <- function (y){                 ## Set function initializes the matrix vaiable to the given value
		x <<- y         
		invX <<- matrix()              ## Sets inverse matirx vaiable to empty matrix
	}
	get <- function() x                  ## Get method to get matrix
	setInverse <- function(m) {          ## Calculates and sets the inverse matrix value
		invX <<- solve(m)
	}
	getInverse <- function(){            ## Gets the inverse materix value
		invX
	}
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) ## list of functions defined for easier access to functions
}


## CacheSolve utilizes the makeCacheMatrix function to calculate and publish the Inverse of a matrix
## If the inverse is already calculated it gets from the cache rather than calculating again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	InvM <- x$getInverse() ## get inverse of the matrix defined in makeCacheMatrix
	if (!all(is.na(InvM))) {      ## if matrix not empty then return the inverse
		message("getting cached data")
		return(InvM)
	}
	Mx <- x$get()                 ## get the orginal matrix
	x$setInverse(Mx)              ## Set the inverse by passing the origincal matrix
	x$getInverse()                ## get the inverse of the matrix
}
