makeCacheMatrix <- function(x = matrix()) {                  #function 'makeCacheMatrix' creates matrix object
        inverse_matrix <- NULL                          #set value "NULL" to the variable, which will consist of inverse matrix 
        set <- function(y) {                                 #function 'set' set value to the variable 'x'
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x                                   #get value of the variable 'x'
        setsolve <- function(solve) inverse_matrix <<- solve   #assignment value of variable 'solve' to variable 'inverse_matrix'  
        getsolve <- function() inverse_matrix                 #get value of the variable 'inverse_matrix'
        list(set = set, get = get,                            #create list of functions
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {                              #'cacheSolve' function computes the inverse of the matrix returned by 'makeCacheMatrix' function
        inverse_matrix <- x$getsolve()                        #get value of inverse matrix, if exists
        if(!is.null(inverse_matrix)) {                        #getting value of inverse matrix from cache
                message("Getting cached data...")      
                return(inverse_matrix)
        } else {
                message("calculating inverse matrix...")      #calculating inverse matrix
                data <- x$get()
                inverse_matrix <- solve(data, ...)
                x$setsolve(inverse_matrix)
        }
        inverse_matrix
}