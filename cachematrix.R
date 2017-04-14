## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it .The functions below cache the inverse of a matrix.


# The first function makeCacheMatrix : creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.


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


# Example

X<-matrix(rnorm(16),nrow=4,ncol=4) #create a 4x4 matrix with random values

test<-makeCacheMatrix(X) #allocate the X matrix to the special matrix

cacheSolve(test) #compute the inverse. Here not yet in cache so compute it

cacheSolve(test) #compute the inverse. This time in cach so just retrieve it






