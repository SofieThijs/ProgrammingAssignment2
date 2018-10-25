################################################################
# The first function creates a list containing a function to   #
#                                                              #
# * set the value of the matrix                                #
# * get the value of the matrix                                #
# * set the value of the inverse                               #
# * get the value of the inverse                               #        
################################################################

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#################################################################
# The second function calculates the inverse of the list created#
# by the first function. But it first checks to see if the      #
# inverse has already been calculated. If so, it gets it from   #
# the cache and skips the computation. Otherwise, it calculates #
# it and sets the value of the inverse in the cache via the     #
# setInverse function.                                          #
#################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
