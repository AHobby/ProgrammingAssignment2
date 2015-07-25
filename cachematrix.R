# Here are a pair of functions that work together to 
# solve and cache the inverse of a matrix. 
# The first one is makeCacheMatrix, and it creates and returns a list of functions.
# The second function (cacheSolve) uses that list of functions to first check to
# see if the matrix has changed. If it has, reset variables. If not,
# see if an inverse of the matrix has been cached.
# If it cannot find a cached inverse,then it will solve to find the inverse.

# makeCacheMatrix creates the necessary variables and functions

makeCacheMatrix <- function(x = matrix()) {
        #set the matrix to NULL
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #Empty function like this just returns the value of whatever was passed to it
        #hence calling it 'get' or 'getinverse' 
        #setinverse assigns the inverse of the matrix to empty matrix m
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## cacheSolve uses the functions from makeCacheMatrix to either get the inverse 
## of the matrix, or to solve for it.

cacheSolve <- function(abc, amatrix= matrix(), ...) {
        # Return a matrix that is the inverse of 'abc'
        # First, check if the matrix has changed.
        # If not, then use set, which will 
        # make the inverse equal to NULL, and ensure that solve is called.

        
        data <- abc$get()
        if (!identical(data, amatrix)) {
                message("Matrix is different, so resetting so can solve for inverse")
                abc$set(amatrix)
        }
        
        m <- abc$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if there is no cached value, then solve for the inverse
        m <- solve(data,...)
        abc$setinverse(m)
        return(m )      
        
        
}

# checked that it was working by generating a 4x4 matrix
# using runif to generate the matrix elements

amatrix <- matrix(runif(16,1,10),4,4)
amatrix
a <- makeCacheMatrix(amatrix)
cacheSolve(a, amatrix)
amatrix <- amatrix + 2
cacheSolve(a, amatrix)

cacheSolve(a)
