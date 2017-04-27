## Put comments here that give an overall description of what your
## functions do

## the function creates the inverse of a matrix and stores in cache.

makeCacheMatrix <- function(x = matrix()) { ##set up function as matrix x
        i <- NULL ##default i before function run       
        set <- function(y) { ##setting value for function       
                x <<- y 
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) ##getting inverse

}


        ## This function processes the inverse of the matrix created from makeCacheMatrix
        ## if the inverse already exists it will return from chache

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("returning cache data")
                return(i)  ##returns cached data
        }
        cs <- x$get()
        i <- solve(cs, ...)  
        x$setInverse(i)
        i
}

