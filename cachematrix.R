## Here is a desciption of what the overall functions do
## Lexical scoping and caching - the makeCacheMatrix and cacheSolve functions 
## allow me to reduce computation time
## They do this by computing within a function and then saving (caching)
## that function for repeated use

## The makeCacheMatrix function computes a special matrix object that can cache its inverse

## Below, sets the value of the m to NULL, caches the inputed matrix object, 
## gets the value of the matrix, gets the inverse of the matrix, 
## and makes a list of all the functions

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y                            ## uses <<- to assign a value to the objects 
            m <<- NULL                         ## in an environment different from this environment
            }
          get <- function()x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special matrix 
## returned by the makeCacheMatrix function above 
## If the inverse has already been calculated and the matrix has not been changed
## then the cacheSolve function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
          m <- x$getinverse()                   ## Get the matrix inverse from x
          if(!is.null(m)) {                     ## if the inverse is present
            message("getting cached inverse")   ## this message is generated
            return(m)                           ## and the computed inverse is returned
          }
          data <- x$get()                       ## if the inverse is not present, we get x,
          m <- solve(data, ...)                 ## we solve x,
          x$setinverse(m)                       ## we set the inverxe to x
          m                                     ## we return the solution
        ## Return a matrix that is the inverse of 'x'
}

## Tested the functions using Rob de Beir's test for functions
## from the forum "Caching in Practice" thread
## Ran his matrix with his results
## Used this example matrix: matrix(c(2, 3, 1, 2), nrow = 2, ncol = 2)
## Here is the result:
## getting cached inverse
##      [,1] [,2]
##[1,]     2   -1
##[2,]    -3    2
