## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix take a matrix as an argument and return a list 
## containing all getters and setters. Along with functions, returned list also 
## have access to all the elements which are free variables for functions but 
## defined in parent environment.
## Reason:
##       getters and setters are defined inside function makeCacheMatrix and when 
##       list is returned, it contains pointer to functions and thus the environment 
##       in which functions are declared not cleaned from memory, and thus variables
##       x and m, though free variables for functions like get, still can access it.
##       This behaviour is direct consequence of Lexical Scoping.

## Write a short comment describing this function

## This function returns R object, a list which contain functions to get matrix 
## itself and its inverse, and functions to set matrix and its inverse.
## Assignment operator '<<-' is used here to assign variable in parent scope.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInv <- function() inv
    setInv <- function(x) inv <<- x
    
    list(set = set, get = get, getInv = getInv, setInv = setInv)
}

## Write a short comment describing this function

## This function takes list returned by makeCacheMatrix as argument and compute 
## the inverse of matrix passed to makeCacheMatrix (default is empty matrix), and
## check if its is already present in cache.
## if TRUE, then it simply return the inverse with a message that it has returned
## from value present in cache.
## otherwise, it get the matrix by 'get' method, compute its inverse, set that 
## value to cache and return the inverse.
## method 'set' can change the matrix, it takes a matrix as an argument

## See 'Example Run:' below to see  how it works.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("From Cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}

## Example run:

## > source("cachematrix.R")
## > mat <- matrix(rnorm(16), nrow = 4, ncol = 4)
## > mat_inv <- makeCacheMatrix(mat)
## mat_inv is a R object which is input to cacheSolve()
## > mat_inv$get()
## returns 'mat'
## > mat_inv$getInv()
## returns NULL because matrix inverse has not been calculated for the first time
## > mat_inv$setInv()
## set matrix inverse calculated first time
## > mat_inv$set(mat2)
## set matrix 'mat2' replaced with 'mat'
## cacheSolve(mat_inv)
## return the required inverse, if already calculated before then returned from 
## cache, else calculated and set by method 'setInv'
