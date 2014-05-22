## 21 May 2014, SNK
## Coursera R Programming class
## Programming Assignment 2
## Matrix inversion is computationally costly.  
## These function cache & retrieve an inverted matrix for later use.

## makeCacheMatrix creates a list of functions (set, get, setinv, getinv)
## that will be used to cache and retrive the inverted matrix in the next section

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set stores the matrix in x, clears a previously cached m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get retrieves the matrix from x
    get <- function() x
    
    # setinv stores the inverted matrix in m
    setinv <- function(inv) m <<- inv
    
    # getinv retrieves the inverted matrix from m
    getinv <- function() m

    # return a list of functions with named indices
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calcultes the inverse of the matrix stored above
## if there is no inverse (m) already cached
## if the inverse has been previously cached and matches,
## it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    ## if m is not empty
    if(!is.null(m)) {
        ##show a message "getting cached data"
        message("getting cached data")
        
        ##check that the input matrix hasn't changed
        a <- x$get()
        b <- x$getinv()
        ## a %*% b should be the identity matrix
        ## create an identity matrix with same dimensions as a
        ident <- diag(nrow(a))
        if(identical(round(a %*% b,2),ident)){
            #return the cached value
            return(m)    
        } else {
            message("cached data does not match")
            }
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


#Notes:
#to invert matrix: solve(matrix)
#to test inversion: matrix %*% solve(matrix) -- should yield identity matrix
#to create an identity matrix (for square matrix): diag(nrow(matrix))
#to test: identical(diag(nrow(matrix)), matrix %*% solve(matrix)))

