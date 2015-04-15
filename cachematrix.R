## Put comments here that give an overall description of what your functions do


## This script is designed to take a value for a square, inversible matrix , calculate its inverse, save & return the inverse.
## If the same matrix inverse is queried again, the value is not calculated again but is retrieved
##       from the cache.
## If the matrix value changes then the inverse is calculated and stored. 
## Script uses assignment operator( <- ) and super-assignment operator ( <<- ) to display lexical scoping
## <-   =  Used for assigning in current environment
## <<-  =  Used for assigning a value to a variable from the parent environment
## Exmaple : variable m is declared in makeCacheMatrix & cacheSolve functions using <-
##           m is also needed for inside the set functions. So have to use <<- operator


## _______________________________________________________________________________________________________

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a list of 4 different functions - set a matrix value,
##        get the matrix value, set an inverse value of that matrix and get the inverse value



makeCacheMatrix <- function(x = matrix()) 
{    m <- NULL
     
     set <- function(y)                                # set the value of a matrix
     {     x <<- y
           m <<- NULL
     }
     
     get <- function() x                               # retrieve the value of the matrix
     setinverse <- function(inverse) m<<- inverse      # set the cacluated inverse
     getinverse <- function() m                        # retrieve the inverse value
     
     list(set =  set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function that first checks if the matrix queried for already exists in cache.
## If it does, it returns the cached inverse
## If it does not exist, then it creates the inverse using the solve function of R and stores it
##    in the cache using the setinverse function within makeCacheMatrix.
## Returns the inverse of matrix 

cacheSolve <- function(x, ...)
{     m <- x$getinverse()                     # get the inverse of the matrix
      
      if(!is.null(m))                         # check to see if inverse exists
      {     message("getting cached data")
            return(m)                         # get inverse from the cache
      }
      
      data <- x$get()                         # calculate the inverse since it doesn't already exist
      m <- solve(data)
      x$setinverse(m)
      m
}
  

