## These two functions allow to calculate the inverse of a given matrix one time only 
## And then retrieve it from a cache if needed

## Creating a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse matrix
    inverse_matrix <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
        x
    }
    
    ## Method to set the inverse of the matrix
    set_inverse <- function(inverse_matrix) {
        i <<- inverse_matrix
    }
    
    ## Method to get the inverse of the matrix
    get_inverse <- function() {
        inverse_matrix
    } 
    
    ## Return a list of the methods
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## Computing the inverse of the special “matrix” returned by makeCacheMatrix above
## If the inverse has already been calculated then retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse()
    
    ## Return the inverse if its already set
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    
    
    ## Get the matrix from the object
    matrix <- x$get()
    
    ## Calculate the inverse matrix
    inverse_matrix <- solve(matrix, ...)
    
    ## Set the inverse to the object
    x$set_inverse(inverse_matrix)
    
    ## Return the matrix
    inverse_matrix
}
