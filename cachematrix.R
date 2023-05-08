## Put comments here that give an overall description of what your
## functions do

## function to create a special matrix that chaches its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set the value of the matrix 
        set <- function(y){
                x <<-y
                m <<- NULL
        }
        #gets val of matrix 
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <-  function() m
        list(set=set,get=get,
             setInv = setInv,
             getInv = getInv)
        
}


## retrieve this cached matrix, or calculate inverse 
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)){
                message('getting cached matrix')
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
