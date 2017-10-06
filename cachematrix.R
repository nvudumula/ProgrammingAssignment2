## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invX <- matrix()
	set <- function (y){
		x <<- y
		invX <<- matrix()
	}
	get <- function() x
	setInverse <- function(m) {
		invX <<- solve(m)
	}
	getInverse <- function(){
		invX
	}
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	InvM <- x$getInverse()
	if (is.null(InvM)) {
		message("getting cached data")
		invM
	}
	Mx <- x$get()
	x$setInverse(Mx)
	x$getInverse()
}
