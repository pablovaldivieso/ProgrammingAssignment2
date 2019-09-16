## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # set the value 
        inv <- NULL
        set <- function(matrix){
                x <<- matrix
                inv <<-null
        }
        #get the matrix
        get <- function(){
                #return the matrix 
                x
        }
        #set the inverse of the matrix
        setInverse <- function (inverse){
                inv <<- inverse
        } 
        #get the inverse
        getInverse <- function (){
                inv
        }
        #Return a list of methods
        list (set =set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #If statement to return the inverse if its already set
        if ( !is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        #get the matrix
        mat <-x$get()
        #Calculate inverse using "solve" function
        inv <-solve(mat,...)
        #set inverse
        x$setInverse(inv)
        #return the matrix
        inv
}

