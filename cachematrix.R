## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 # takes no.of rows and no.of columns as argument and creates special "matrix" which can set / get matrix , set/get inv from cache
makeCacheMatrix <- function(nrows,ncols,...){
    inv<<- NULL    
    set <- function(nrows, ncols,...)
    {
      if(nrows!=ncols)  #checks for squarematrix
      {
        message("not invertible")
        return
      }
      mat <<- matrix(sample.int((nrows*ncols+100),nrows*ncols),nrows,ncols) #generate random numbers and create matrix
      inv <<- NULL
    }
    get <- function() mat  #this funciton returns matrix
    setinv <- function(inv) inv<<- inv #this function caches inverse
    getinv <- function() inv  # this function returns inverse
    list(set=set, get=get, setinv = setinv, getinv = getinv) 
}

#this funciton returns inverse from cache / calculates inverse if already cached / matrix obj changed
cacheSolve <- function(matobj, ...)
{
  inv <- matobj$getinv()
  if(!is.null(inv))
  {
    message("getting cached data") #if the inverse is already calculated then returns data from cache
    return(inv)
  }
  data <- matobj$get() #calculate inverse if not already in cache/matrix obj changed
  inv <- solve(data)
  matobj$setinv(inv) 
  inv
}
