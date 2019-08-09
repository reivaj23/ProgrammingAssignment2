## My script for Coursera, R Programming Week, Programming assignment #2
## The pair of functions receive a matrix, computes its inverse, and stores the value of the inverse
## in cache memory so it can be accessed without having to compute it again.

## USAGE:
## 1. call the makeCacheMatrix, with a matrix as an argument, and save the output in a variable. Example:
##      myMatrix <- makeCacheMatrix(matrix(sample(25), nrow=5))
## 2. call the cacheSolve() function, with the variable created in the previous step as argument. Example:
##      cacheSolve(myMatrix)
## Inverse of the matrix created in step #1 will be displayed, and stored for future use. If cachaSolve(myMatrix) is called
## again, for the same matrix, a message will be displayed indicating the inverse was obtained from cached data.
## Use myMatrix$set to set a new matrix. Example: myMatrix$set(matrix(sample(36), nrow=6))


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


