## Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly . 
##This program contains a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.
##set the value of the vector
##get the value of the vector
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        
        get<-function() x
        setinv<-function(inverse) i<<- inverse
        getinv<-function() i
        list(set=set, get=get,
             setinverse=setinv,
             getinverse=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrix<-x$get()
        i<-solve(matrix)
        x$setinv(i)
        i
}

