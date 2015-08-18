## The following is a pair of functions that cache the inverse of a matrix
## 
## makeCacheMatrix creates a blank matrix using 4 functions that get and store vectors. 
#Aim of the function is return the value of the inverse of a vector
makeCacheMatrix <- function(x = matrix()) { 
    m<-NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
}      


#This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
#If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve 
#should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

