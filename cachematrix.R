## We are writting a pair of functions to cache a computational operation 
## which we might want to avoid computing it several times. In this case,
## the computational operation is the inverse of a squared matrix.

## Function makeCacheMatrix will check for the following: set the value
## of a matrix, get the value of a matrix, set the value of the inverse and 
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function cacheSolve will compute the inverse only if it has not been computed
## previously. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Example

# defining a random 4x4 matrix 
my_vec <- runif(16,0, 5)
dim(my_vec) <- c(4, 4)
my_vec

# compute inverse
mat <- makeCacheMatrix(my_vec)
inverse_mat <- cacheSolve(mat)

# compute identity with inverse
round(my_vec %*% inverse_mat, 1)

# if we try to compute the inverse again...
print(another_inverse_mat <- cacheSolve(mat))