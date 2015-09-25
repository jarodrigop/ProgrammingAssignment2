## to test:
## open R
## > source "cachematrix.R"
## create a matrix, by example:
## > matrixtest1 <- matrix( rnorm(25), nrow=5, ncol=5)
## make cache matrix:
## > mt1 <- makeCacheMatrix(matrixtest1)
## an execute:
## > cacheSolve(mt1)
## when execute again, the data is obtain by cache
## > cacheSolve(mt1)
## > getting cached data


makeCacheMatrix <- function(x = matrix()) {

## makeCacheMatrix create a list of functions:
## - get
## - put
## - setInv
## - getInv
    
    ## invx inverse matriz
    invx = NULL

    ## set the matrix to object create by makeCacheMatrix
    set <- function(y) {
        ##
        x <<- y
        invx <<- NULL
    }

    ## get the matrix to return the input matrix
    get <- function() x

    ## set the inverse matrix 
    setInv = function(inverse) invx <<- inverse

    ## get the inverse matrix
    getInv = function() invx

 
    ## Create list of arguments
    list (set=set, get=get, setInv=setInv, getInv=getInv)

}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    matrixInvCache = x$getInv()

    # if exists a cached inverse, the return it. Else, calculate the inverse and set to be cached.

    if (!is.null(matrixInvCache)) {
        ## return a message
        message("getting cached data")
        ## return the value
        return(matrixInvCache)
    } else {
        ## get the matrix
        matrixData <- x$get()
        ## calculate the inverse by solve
        matrixInv <- solve(matrixData)
        ## set the inverse to be cached
        x$setInv(matrixInv) 
        ## return the value
        return(matrixInv)
    }

}
