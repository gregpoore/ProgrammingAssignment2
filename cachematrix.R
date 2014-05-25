		# Purpose: to cache the matrix's inverse
		# so that repeated calculations can be minimized
		# Note: this function's output is a list of other functions
		# which can be accessed via scoping
makeCacheMatrix <- function(x = matrix()){
       inv <- NULL # initialize the default inverse value to NULL
       set <- function(y) {
              		# Note: setting the matrix creates it
              		# and hence the inverse is not known up front, or NULL
              		# the <<- brings the value of x and inv into the parent context,
              		# or the environment of makeCacheMatrix
              x <<- y
              inv <<- NULL
       }
       get <- function() x # after setting the matrix, calling get retrieves it for
                            # use in cachesolve
       setinv <- function(inverse) inv <<- inverse
       	 # setinv takes the inverse as an input and "sets" it as inv in the global context
       getinv <- function() inv # by calling getinv, the inverse matrix is retrieved
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
       # this list is the final output of makeCacheMatrix
       # and summarizes it as a list of functions
}


       # Purpose: to check if our desired inverse is already in the cache
       # and if so, display it to the user; otherwise, calculate inverse of ‘x’
       # Note: the x argument is an object created by makeCacheMatrix (a list)
       # and not a simple matrix; this allows the calling found below such as x$get()
cachesolve <- function(x, ...) {
       inv <- x$getinv() # the default value for inv is NULL, unless it has been set
                            # previously by the makeCacheMatrix function
       if(!is.null(inv)) {
              		# if the value is not null, then the inverse is already stored in the cache
              		# and simply needs to be returned to the user
              message("Getting cached data...")
              return(inv) # if available in the cache, the function stops and returns the value
       }
       data <- x$get()
       		# reaching this point in the function means that the inverse was not previously cached
       		# hence, the data is retrieved from the list object
       inv <- solve(data, ...) # using the data, the matrix inverse is found
       x$setinv(inv) # lastly, the new matrix inverse is set as a cached value
                     # this will allow future iterations to compare against it
       inv
}
