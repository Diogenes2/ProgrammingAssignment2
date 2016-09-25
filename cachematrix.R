## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix takes in a matrix and produces an output that makes the environment work with the
## cacheSolve function to have it read or calculate the inverse of a matrix. 

## Write a short comment describing this function 
## The function takes in a matrix and outputs a list of functions, not a matrix. The function prepares everything so cacheSolve can
## work simply. When it sets n to null it makes it so after the first time cacheSolve can avoid the solve function and just return.

makeCacheMatrix <- function(j = matrix()) {
        n <- NULL
        mset <- function(y) {
                j <<- y
                n <<- NULL
        }
        mget <- function() j
        setmatrix <- function(solve) n <<- solve
        getmatrix <- function() n
        list(mset = mset, mget = mget, setmatrix = setmatrix, getmatrix = getmatrix)
}



## Write a short comment describing this function
## After makeCacheMatrix sets up the environment for cacheSolve to work, cacheSolve sees if n is null, and if it isn't null it 
## just returns n. If n is null the inverse is calculated and then it outputs the inverse of the original matrix that was the 
## input for makeCacheMatrix.
cacheSolve <- function(j, ...) {
        n <- j$getmatrix()
        if(!is.null(n)){
                message("getting cached data")
                return(n)
        }
        data <- j$mget()
        n <- solve(data, ...)
        j$setmatrix(n)
        n
        
