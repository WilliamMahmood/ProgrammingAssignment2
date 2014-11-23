# makeCacheMatrix() & cacheSolve(). 


# // makeCacheMatrix()

# function definition: 

	# creates a special "matrix" object that can cache it's inverse.

# function summary: 

	# specifically for a matrix it will a) set/get its value and b) set/get the value of its INVERSE.

# function code...

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the stored inverse value to NULL...
  inverse <- NULL
  
  # set the value of the matrix...
  set <- function(y) {
    x <<- y
    inverse <<- NULL   # NULL again because the matrix has changed...
  }
  
  # get the value of the matrix...
  get <- function() x
  
  # set the inverse...
  inverse.set <- function(inv) inverse <<- inv
  
  # get the inverse...
  inverse.get <- function() inverse
  
  # return a list of all the above functions...
  list(set = set, get = get, inverse.set = inverse.set, inverse.get = inverse.get)    
}


# // cacheSolve()

# function definition: 
	
	# cacheSolve computes the inverse of the "matrix" from makeCacheMatrix(). 

# function summary: 

	# if() tests if the inverse is already cached...
	# if inverse is cached THEN return the cached inverse...
	# if inverse is NOT cached THEN compute the inverse...
	# the function always notify's the user where the inversed matrix came from (cache or computed). 

# function code... 

cacheSolve <- function(x, ...) {
  # test if the inverse is already cached...
  inverse <- x$inverse.get()
  if(!is.null(inverse)) {
    message("returned from cache...")
    return(inverse)
  } else {
    message("computed on the fly...")
    
    # not cached, so we get the matrix into data
    data <- x$get()
    
    # and compute the inverse
    inverse <- solve(data, ...)
    
    # then cache the inverse
    x$inverse.set(inverse)
    
    # and return it as well
    inverse
  }
}


# // appendix: testing

# create and set matrix...

x <- rbind(c(1, -0.5), c(-0.5, 1))

m <- makeCacheMatrix(x)

# compute for first time 

> cacheSolve(m)
computed on the fly...
          [,1]      [,2]
[1,] 1.3333333 0.6666667
[2,] 0.6666667 1.3333333


# computed again (this time from cache)

> cacheSolve(m)
returned from cache...
          [,1]      [,2]
[1,] 1.3333333 0.6666667
[2,] 0.6666667 1.3333333