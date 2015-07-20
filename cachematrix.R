## Here are a pair of functions that work together to solve and cache the inverse of a matrix. 
## The first one is makeCacheMatrix, and it creates and returns a list of functions.
## The second function (cacheSolve) uses that list of functions to first check to 
## see if an inverse of the matrix has been cached.
## If it cannot find a cached inverse,then it will solve to find the inverse.

## makeCacheMatrix creates the necessary variables and functions

makeCacheMatrix <- function(x = matrix()) {
        #set the matrix to NULL
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        
        #get the value of x using the anonymous function. 
        #Empty function like this just returns the value of whatever was passed to it
        #hence calling it 'get' or 'getinverse' 
        #setinverse sets the inverse of the matrix to variable m
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
       
        
}


## cacheSolve uses the functions from makeCacheMatrix to either get the inverse 
## of the matrix, or to solve for it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if there is no cached value, then solve for the inverse
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m       
        
        
}

amatrix <- matrix(runif(16,1,10),4,4)
amatrix
a <- makeCacheMatrix(amatrix)
cacheSolve(a)


?solve
solve(amatrix)
