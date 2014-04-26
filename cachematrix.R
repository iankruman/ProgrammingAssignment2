## makeCacheMatrix and cacheSolve work in conjunction to solve matrix inverses.
## makeCacheMatrix creates a special list of functions which store cached matrix and inverse values.
## cacheSolve calls the functions in this list and returns a matrix's inverse, 
## performing the inverse calculation only if the inverse hasn't already been calculated.


## makeCacheMatrix takes a matrix as its input and returns a list of four
## functions used for getting and setting cached matrix and matrix inverses.

makeCacheMatrix <- function(cacheMatrix = matrix()) {

        ## Initalize the cached inverse value to NULL.
        cacheInverse <- NULL

        ## Create a function to set the cache matrix.
        ## When the cache matrix is set, the cache inverse is set to NULL
        ## because the inverse will need to be recalculated.
        set <- function(inputMatrix){
                cacheMatrix <<- inputMatrix
                cacheInverse <<- NULL
        }

        ## Create a function to get the cache matrix.
        get <- function() cacheMatrix

        ## Create a function to set the cache inverse.
        setInverse <- function(inverse) cacheInverse <<- inverse

        ## Create a function to get the cached inverse.
        getInverse <- function() cacheInverse

        ## Return a list containing the 4 functions.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve takes as input 'x', the list of functions created by makeCacheMatrix.
## It first checks if the inverse of x's cached matrix is NULL.
## If the inverse is not NULL, it returns the cached inverse.
## If the inverse is NULL, it calculates the inverse, sets it in x's
## cache, and returns the calculated inverse.

cacheSolve <- function(x, ...) {

        ## Get x's cached inverse value.
        inverse <- x$getInverse()

        ## If x's cached inverse value is not null, return cached inverse of x.
                if(!is.null(inverse)){
                message("getting cached inverse")
                return(inverse)
        }

        ## For x with NULL inverse value, calculate cached matrix's inverse.
        inverse <- solve(x$get(), ...)

        ## Set the value of the inverse in x's cache.
        x$setInverse(inverse)

        ## Return a matrix that is the inverse of 'x'.
        inverse
        
}
