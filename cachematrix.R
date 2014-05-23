## Below are a pair of functions. I hope they work, but I make no 
## guarantees. Ieally, this pair of functions will cache time-consuming
## computations. Let's see how I did!


## This first function will create a matrix that can cache its inverse. 
## I **think**

makeCacheMatrix <- function(x = matrix()) {
      #set m to be NULL
      m <- NULL
      ## the first function - set - will assign an input to x
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## the second function - get - will return the value of x
      get <- function() x
      ## the third function saves the inverse
      setinv <- function(inv) m <<- inv
      ## the fourth function returns the value of the inverse
      getinv <- function() m
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}


## This second function will create compute the inverse of the matrix
## returned by the first function above. If it has already been computed, 
## this function will retrieve the inverse from the cache rather than
## re-calculate it. 

cacheSolve <- function(x = makeCacheMatrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      ## first, return the value of the inverse (it could be NULL)
      m <- x$getinv()
      ## if it is not null, then return 'm', which is the inverse, stored 
      ## from above
      if(!is.null(m)) {
            message ("getting cached data")
            return(m)
      }
      ## if it is null, then we need to calculate the inverse
      ## to do that, first return the matrix for which we want to calculate
      ## the inverse. Do that by running 'get' from above
      data <- x$get()
      ## then, assign the inverse of that matrix to 'm'
      m <- solve(data)
      ## then, use 'setinv()' on m to store the inverse matrix in the cache
      x$setinv(m)
      ## finally, return the value of 'm', which is the value of the inverse
      ## of the matrix we wanted to calculate
      m
}
