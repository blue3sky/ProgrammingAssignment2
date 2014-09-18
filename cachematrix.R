## This file contains two functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix: This function creates a special "matrix" object which is a list 
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the
## cache via the setmatrix function.
## The matrix must be invertible (non-singular), meaning 
## determinant cannot be equal to zero for the function to work properly.

## This function takes in a matrix and perform the required function
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This function takes in a matrix and return the inverse of the matrix.
## If the inverse has already been cached, the cached inverse will be returned.
## Else, the inverse will be calculated, cached and returned. 
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
