## This file contains a pair of functions that cache the inverse of a matrix.
## Programming Assignment 2 
## Submitted by John Paul Aldeza, PH

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL 			#setting the inverse to NULL
	set<-function(y){		#defines a function 
                x<<-y			#to set the matrix, x, to a new matrix, y
                inv<<-NULL		#and resets the mean, m, to NULL
      }
	get<-function() x		#returns the matrix, x
	setInverse<-function(inverse) inv<<-inverse		#sets the inverse, inv, to inverse
	getInverse<-function() inv				#returns the inverse, inv
	list(set=set,			#returns the 'special matrix' containing all of the functions defined above
	     get=get,
	     setInverse=setInverse,
	     getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv<-x$getInverse()
	if(!is.null(inv)){		#checks if the inverse was already chached
		message("getting cached data")
		return(inv)
	}
	data<-x$get()			#gets the matrix and assigns to data
	inv<-solve(data,...)		#gets the inverse of matrix, data, using solve()
	x$setInverse(inv)
	inv				#returns the inverse
}
