## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix provides the lexical scoping functions
## that are return as a list(), this includes the set/get for the cache value
## and setSolve/getSolve 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #getter function, gets input non-inversed matrix
    get <- function() x
    
    #to cache to inversed matrix
    setInmatrix <- function(Inmatrix) m <<- Inmatrix
    
    getInmatrix <- function() m
    
    #return list of four functions when makeCacheMatrix is called
    list(get = get, setInmatrix = setInmatrix, 
         getInmatrix = getInmatrix)
}


## Assuming provided is always an invertible matrix,
## function first param is matrix, followed by other variables.
#   1. load cahce, if not null then return cache of x
#   2. if null, get supplied matrix from "get", perform inverse
#   and cache for later. 
cacheSolve <- function(x, ...) {
    
    m <- x$getInmatrix() #load cached inverse matrix
    
    #return immediately matrix if found
    if(!is.null(m)){
        message("returning cached matrix")
        return(m)
    }
    
    message("No cache data found, data will be inverted and cached")
    data <- x$get()     #supplied matrix is retreived and set as "data"
    m <- solve(data, ...)   #inverse matrix is generated
    x$setInmatrix(m)   #goes into function and sets the matrix as cache
    m   # return inverse matrix
}
