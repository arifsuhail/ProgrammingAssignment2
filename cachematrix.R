## use the following function to make a special matrix
## for example you may write the following for 3x3 matrix as below
##the numbers are shown just as an example
## a<- makeCacheMatrix(matrix(c(1,5,8,3,8,9,2,4,7),3,3))

makeCacheMatrix <- function(x= matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##After, for example, a is made as above, you may use the following function
##to get the inverse of the matrix by calling as
## cacheSolve(a)
##if you use again cacheSolve(a), you will get the cached value

cacheSolve <- function(x, ...)
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
