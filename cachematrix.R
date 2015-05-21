## The following functions cache and compute the inverse of a matrix.
## The first one, makeCacheMatrix, creates a special "matrix" object.

makeCacheMatrix<-function(m=matrix()) {
  inverse<-NULL
  setmatx<-function(y) {
    m<<-y
    inverse<-NULL
  }
  getmatx<-function()m
  setmatinv<-function(inv)  inverse<<-inv
  getmatinv<-function() inverse
  list(setmatx=setmatx,getmatx=getmatx,setmatinv=setmatinv,getmatinv=getmatinv)
}

## The following function, cacheSolve, computes the inverse of the special 
## "matrix" returned by "makeCacheMatrix" above. If the inverse has 
## already been calculated because the matrix has not changed, then cacheSolve
## retrieve the inverse from the cache saying "getting cached data"

  cacheSolve<-function(m,...) {
    inverse<-m$getmatinv()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data<-m$getmatx()
    inverse<-solve(data,...)
    m$setmatinv(inverse)
    inverse  
  }
  
