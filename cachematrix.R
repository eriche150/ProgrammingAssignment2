makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #undefined value 
        set <- function(y) { #embedded another function within makeCacheMatrix
                x <<- y
                inv <<- NULL
        }
        get <- function() {x} 
        setInverse <- function(inverse) {inv <<- inverse} #this is the value of the inverse matrix
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}

cacheSolve <- function(x,...) {
        inv <- x$getInverse() 
        if (!is.null(inv)) { #if inv is NOT NULL 
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}