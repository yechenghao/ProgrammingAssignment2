## Put comments here that give an overall description of what your
## functions do

## This is a constructor function that creates a "container" to store the value of the matrix 
# and it's inverse. The values of the matrix and it's inverse are only accessible through member
# functions of set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        
        #These section defines functions that have access to the variables x, and x_inverse.
        #x and x_inverse are considered free variables to the functions to be defined within
        #makeCacheMatrix. Because R uses lexical scoping, the functions defined within 
        #makeCacheMatrix have to access these free variables as they are in the environment 
        #in which the functions were defined.
        
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(m_inverse) x_inverse <<- m_inverse
        getinverse <- function() x_inverse
        
        #This stipulates the list that is to be returned by makeCacheMatrix function
        #the list labels have the same names as the function names
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve is a function that takes in the list object created by the makeCacheMatrix function. 
## cacheSolve accesses the inverse matrix stored in cache of the list object. 
## If null, it computes the inverse matrix. If not null, it will return the inverse matrix stored
## in cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # checking to see if the inverse matrix is already available
        # if available, to return value without additional computation
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)){
                message("getting cached data")
                return(m_inverse)
        }
        
        #this section is skipped if an inverse matrix had previously been cached
        #if no cached matrix exists, to compute its inverse, store it in cache, and return
        #the value of the inverse matrix
        dataMatrix <- x$get()
        m_inverse <- solve(dataMatrix)
        x$setinverse(m_inverse)
        m_inverse
}
