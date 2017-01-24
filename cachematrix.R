makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
# creating the matrix 
        
        set <- function(y) {      
                x <<- y
                cache <<- NULL
        }

        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache

        # return the created functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## get the inverse of the matrix stored in cache
        cache <- x$getInverse()

        ## return inverted matrix from cache if it exists else create the matrix in working environment
        
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }

        # create matrix since it does not exist
        matrix <- x$get()

       
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning")
                message(e)

                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )

        return (cache)
}


## To check the result

##  source("cachematrix.R")    load R program
##  x<- makeCacheMatrix()     create functions
##  x$set(matrix(1:4, 2, 2))   create matrix in working environment
##  cacheSolve(x)              1st run returns inverted matrix
##                              from working environment
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##  cacheSolve(x)              2nd and subsequent runs
##                              returns inverted matrix from cache
## getting cached data          
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5