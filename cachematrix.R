## create a special object that stores a matrix and cache its inverse


## This functon create a vector containging four functions:
## set the matrix of the vector
## get the matrix of the vector
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Calculate the inverse of matrix created with makeCacheMatrix
## Check whether inverse has already been caculated
## If so, get inverse and skip computation
## Otherwise, calculate inverse and set in cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            message('getting cached data')
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}
