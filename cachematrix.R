## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function takes a matrix as an input and produces a special output  
## wrapper matrix object which is a list of four functions get,set,getinverse,setinverse
##to cache and retrieve the matrix and its inverse.

##cacheSolve:This function returns inverse of special wrapper matrix object returned by 
##makeCacheMatrix function.If the inverse is already in cache,it'll return that.
##Otherwise computes the inverse and set the inverse via the setinverse method 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	 ##Creates a special wrapper matrix object that can cache its inverse
								
	  m_inverse<- NULL
        set <- function(y) { 
		x<<-y
		m_inverse<<-NULL
	  }
        get <- function() x 
        setinverse <- function(inv) m_inverse <<- inv
        getinverse <- function() m_inverse
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returns inverse of 'x',a special matrix object returned by makeCacheMatrix
	 
	 m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
	  }
        matrixData <- x$get()
        m_inverse <- solve(matrixData, ...)
        x$setinverse(m_inverse)
	  m_inverse

}
