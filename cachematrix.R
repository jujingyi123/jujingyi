## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## his function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n<-NULL
  set<-function(y){
    x<<-y
    n<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) n<<-inverse
  getInverse<-function() n
  list(set=set,get=get,
       setInverse=setInverse,getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(n)
  }
  mat<-x$get()
  n<-solve(mat,...)
  x$getInverse(n)
  n
}

