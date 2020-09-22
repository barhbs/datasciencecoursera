
make_CacheMatrix <- function(x = matrix()) {
      inv <- NULL                       #starting inverse as NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x                 #function to get matrix x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function()              #function to get inverse of the matrix x
    list(set - set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function is used to make and get cache data

cacheSolve <- function(x, ...) {        ## obtain cache data
  inv <- x$getinverse()
  if(!is.null(inv)){                    #checking whether inverse is NULL
    message("getting cached data")
    return(inv)                         #return the value of the inverse
  }
  mat <- x$get()
  inv <- solve(mat, ...)                #calculate the inverse value
  x$setinverse(inv) 
  inv                                   #return the inverse of matrix 'x'
}
