
#################################################
## Programming Assignment 2: Lexical Scoping 
#################################################


## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation.
## However, there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Create two R functions that cache the inverse of a matrix.

## The first function is called makeCacheMatrix. 
## This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {     ## define the argument with default mode of "matrix"
        inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
        set <- function(y) {                    ## define the set function - assigns new values to the matrix x stored in the main function (makeCacheMatrix) in the parent environment 
                x <<- y                         ## hence x <<- y
                inv <<- NULL                    ## if there is a new matrix, reset inv to NULL
        }
        get <- function() x                     ## define the get fucntion - returns the matrix x stored in the parent environment
        setinverse <- function(inverse) inv <<- inverse         ## define the setinverse function -  assigns value of inv in parent environment
        getinverse <- function() inv                            ## define the getinverse - gets the value of inv where called
        list(set = set, get = get,                              ## you need this list in order to refer to the functions with the $ operator
             setinverse = setinverse,
             getinverse = getinverse)
        
}
 

## Create a new R function called cacheSolve.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),  
## the cacheSolve should retrieve the inverse from the cache.                        


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}



### Testing my functions

## Specify a matrix as a 2x2 matrix of the values 1:4 and assign makeCacheMatrix to this matrix.
## Store this as the object my_test 
my_test <- makeCacheMatrix(matrix(1:4, 2,2))

## To look at the original matrix, use the get subset of the makeCacheMatrix function.
my_test$get()

## To get the inverse of the matrix, calculating the first time
cacheSolve(my_test)
## The resulting inverse matrix can now be cached later.

## Run the cacheSolve function again.
cacheSolve(my_test)

## This time it uses the cached information.
## We have also instructed to print the message "getting cached data" when using cached data.
## This will print the second time we run cacheSolve(my_test). 





