## The couple of functions bellow compute and cache inverse matrix computations

## This funcion sets/gets the matrix (on x) and sets/gets the inverse (on m)
makeCacheMatrix <- function(x = matrix()) {
        ## cleans cached matrix variable
        m <- NULL
        
        ## Sets the matrix value and cleans the matrix cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the matrix data
        get <- function() x
        
        ## Sets the cached matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## Gets the cached matrix
        getinverse <- function() m
        
        ## List all the available funcions within this function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates or returns from cached results the inverse matrix store on the recieved parameter (matrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Gets the cached inverse matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If it does not exists, calculates the inverse matrix
        data <- x$get()
        m <- solve(data)
        ## Sets the cached inverse matrix to the result and returns it.
        x$setinverse(m)
        m
}
