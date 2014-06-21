##Basically, there are two functions in this document. The first is the makeCacheMatrix.
##It creates the special  "matrix" object that caches the inverse of a matrix.

##It is important to note that the makeCacheMatrix function is a list 
##containing the following function that: set the value of the function,get the value of the function,
##set the value of the inverse matrix and get the inverse of the matrix.

##More so, there is the cacheSolve function. This function calculates the inverse
##of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated(and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache. 

##The cacheSolve function first checks to see if the inverse of a matrix has already been calculated. 
##If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse matrix of the data 
##and sets the value of the inverse matrix in the cache via the setinverse function.

## The makeCacheMatrix function creates a special "matrix" object 
##that can cache its inverse
	makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) j <<- inverse
        getinverse <- function() j
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

##The cacheSolve function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	j <- x$getinverse()
        if (!is.null(j)){
                message("returning cached data")
                return(j)
        }
        data <- x$get()
        j <- solve(data, ...)
        x$setinverse(j)
        j
}
