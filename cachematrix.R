#Coding Assignment 2 - matrix inversion and caching
#David Heeszel - dschm4@gmail.com (July 24, 2016)

##Code to calculate and cache the inverse of a matrix as described in the course materials


### NOTE: This is basically the same routine described in the problem description but modified for a matrix and its inversion
makeCacheMatrix <- function(x = matrix()) {
      ## function to creates a 'matrix'
      ## specify that the value of inv is null
      inv <- NULL
      #establish important functions
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      #list of functions contained here
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )
}




cacheSolve <- function(x, ...) {
##cachesolve is a function that will, using the ouput of makeCacheMatrix determine if a matrix's inverse exists and if it doesn't calculate it - otherwise it returns the cached value
      ## Return a matrix that is the inverse of 'x'
      ## 
      ## Check if inv is NULL
      inv <- x$getinverse()
      if (!is.null(inv)) {
            # inverse already exists
            message("Inverse cached, getting that!")
            return(inv)
      }
      #inverse doesn't exist - make it
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
