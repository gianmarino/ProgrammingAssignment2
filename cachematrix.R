

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse
  inverseMatrix<-NULL
  newMatrix<-x
  
  set<-function(n){
    newMatrix<<-n
    inverseMatrix<<-NULL
  }
  
  get<-function() newMatrix
  
  setInv<-function(y) inverseMatrix<<-y
  
  getInv<-function() inverseMatrix
  
  list(get=get,set=set, setInv=setInv,getInv=getInv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  storedInverse<-x$getInv()
  
  if(!is.null(storedInverse)){
    print("getting cached data...")
    return(storedInverse)
  }
  
  else{
    invToCalc<-x$get()
    calcMatrix<-solve(invToCalc)
    x$setInv(calcMatrix)
    calcMatrix
  }
}
