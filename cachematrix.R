## Functions to compute and cache the inverse of a matrix

makeCacheMatrix <- function(m = matrix()) {
        ## Initialises the null inverse
        i <- NULL
        
        ## Sets the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        ## Gets the matrix
        get <- function() m
        
        ## Sets the inverse of matrix
        set_inv <- function(inverse) i <<- inverse
        
        ## Gets the inverse of matrix
        get_inv <- function() i
        
        ## List of methods
        list(set = set, 
             get = get, 
             set_inv = set_inv, 
             get_inv = get_inv)
}


## Calculates inverse of matrix, if no inverse exists in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
}

