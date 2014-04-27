## Programming Assignment Week 3
## These two functions,'makeCacheMatrix' and 'cacheSolve', are used to take in variables 
## and store some useful pre calculated properties in the global environment for future reference.
## This is useful as it may reduce repetition of these calculations as you can just refer back 
## to these pre calculated values directly. 


## The 'makeCacheMatrix' function takes the matrix 'x', and returns the inverse of 'x' 
## to the cache.
makeCacheMatrix <- function(x = matrix()) 
{
  ## This function creates a special "matrix" object that can cache its inverse.
  
  
  ## Create a NULL object called 'm'.
  m <- NULL
  
  ## Create a function called 'set' which takes in the input 'y', and assigns it as a 
  ## variable called 'x' in the global environment. Also, set 'm' as a variable in the global 
  ## environment, with nothing in it as the input matrix may have changed. 
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  ## Create a function called 'get' which takes no inputs, and just returns the 'x' vector as 
  ## already stored. 
  get <- function() x
  
  ## Create a function called 'setinv' which takes in the input and assigns it to the 
  ## object named 'm' in the global environment as the inverse matrix of the input. 
  setinv <- function(solve) m <<- solve
  
  ## Create a function called 'getinv' which takes no variables, and just prints the 'm' matrix as 
  ## already stored. 
  getinv <- function() m
  
  ## Sets the list row names to 'set', 'get', 'setean' and 'getean'.
  ## Assign each object created above to the corresponding name. 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
  
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) 
{
  ## ## Return a matrix that is the inverse of 'x'
  
  ## Set 'm' as the output of the function 'getinv' of the vector x. 
  m <- x$getinv()
  
  ## If the variable 'm' is not NULL, then print a message, and return the variable 'm'.
  ## This will return the pre-calculated inverse if it exists. 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Set 'data' as output of the function 'get' of the vector x. 
  data <- x$get()
  
  ## Calculate the inverse of the 'data' matrix sourced above, and assign to 'm'.
  m <- solve(data, ...)
  
  ## Implicitly print the output of 'setinv' function of the object 'm'. 
  x$setinv(m)
  
  ## Implicitly print the vector 'm'
  m
    
}

## Thanks for reviewing my code, have a nice day!!! 
