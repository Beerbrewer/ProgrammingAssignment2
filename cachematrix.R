## This function tries to cache the Inverse of a Matrix
## by creating a matrix object, computing the inverse of the matrix, 
## and retrives or claclultes the inverse from the cashe.

## this first part creates the matrix (which we assume is always invertible)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## here we compute the inverse of the matrix (solve (x))

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
        return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...) ##solve returns the inverse
  x$setmatrix(m)
  m
}
