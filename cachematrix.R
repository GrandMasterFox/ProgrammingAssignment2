## The following functions are meant to create a cached matrix object that 
## will return the inverted matrix. This can be extremely useful in the event of a large matrix
## that we repeatedly need to check its invert by caching it once so it is never calculated more than once


## First, we'll define our cachable matrix object called 'makeCacheMatrix'
## We will give it 4 functions:
##    get -> Rerturn the matrix
##    set -> Set the value of the matrix
##    getInvertedMatrix -> Returns the value of the cached Inverted Matrix
##    setInvertedMatrix -> Set the value of the cacged Inverted Matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachedInvertedMatrix <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInvertedMatrix <- function(invertedMatrix) cachedInvertedMatrix <<- invertedMatrix
  getInvertedMatrix <- function() cachedInvertedMatrix
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## Next we will build our solved caching function that will return the potentially cached Inverted Matrix
## We will first check to see if the Inverted Matrix already exist in the cache
## If true, we will instantly return it
## If not, we will calculate it using the function Solve then we will set the caching of it
## so that we won't need to calculate it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedInvertedMatrix <- x$getInvertedMatrix()
  if(!is.null(cachedInvertedMatrix)) {
    message("getting cached data")
    return(cachedInvertedMatrix)
  }
  data <- x$get()
  cachedInvertedMatrix <- solve(data,...)
  x$setInvertedMatrix(cachedInvertedMatrix)
  cachedInvertedMatrix
}
