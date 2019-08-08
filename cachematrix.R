## My script for Coursera, R Programming Week, Programming assignment #2
## The pair of functions receive a matrix, computes its inverse, and stores the value of the inverse
## in cache memory so it can be accessed without having to compute it again.

## makeCacheMatrix function receives a matrix, and initializes the variable x (received as parameter) and m.
## It also defines the set, get, setinverse, and getinverse, to be used later by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function receives a matrix, and check if the value of its inverse is stored in cache memory.
## If it is, then retrives it without calculating it again. If it is not, then it goes ahead and calculates it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


# Test:
#x <- matrix(c(4,2,7,6), nrow=2)
# aMatrix <- makeCacheMatrix(matrix(sample(4), nrow=2))
# aMatrix$get()
# aMatrix$getinverse()
# aMatrix$set(x)
# cacheSolve(aMatrix)
# aMatrix$getinverse()
# cacheSolve(aMatrix)

