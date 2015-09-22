## A pair of functions that cache the inverse of a matrix as matrix inversion
## can be costly computationally.

## Creates a special "matrix" object that can cache its inverse. Includes functions
## to:
## 1. Set the value of the matrix - set
## 2. Get the value of the matrix - get
## 3. Set the value of the inverse matrix - set_inverse
## 4. Get the value of the inverse matrix - get_inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv_x <<- inverse
        get_inverse <- function() inv_x
        list(set = set, get = get, set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the
## cache and displays a message indicating that fact.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        my_matrix <- x$get()
        inv_x <- solve(my_matrix, ...)
        x$set_inverse(inv_x)
        inv_x
}
