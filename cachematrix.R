## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) { ## define the argument with "matrix"
    invr = NULL                             ## initialize invr as NULL to hold value of matrix inverse 
    set = function(b) {                    ## define the set function to assign new 
        a <<- b                             ## value of matrix in the parent environment
        invr <<- NULL                        ## if there is a new matrix, reset invr to NULL
    }
    get = function() a                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse = function(inverse) invr <<- inverse  ## assigns value of invr in the parent environment
    getinverse = function()	invr                     ## gets the value of invr where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
                                                                                  
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
## Return a matrix that is the inverse of 'a'

 invr = a$getinverse()
    if(!is.null(invr)) {
        message("obtaining cached data")
        return(invr)
    }
    data = a$get()
    invr = solve(data, ...)
    a$setinverse(invr)
    invr
        
}
