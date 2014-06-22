## makeCacheMatrix() pega uma matriz como parametro e retorna 
## uma lista de funções para manipular a matriz e sua 
## matriz inversa, o qual é modificado pelo cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(xinv) inv <<- xinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve() pega uma matriz cache (produzido pelo makeCacheMatrix()) e
## retorna ambos uma inversa cache ou um novo recalculo 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("obtendo inversa cached")
    return(inv)
  }
  message("calculando a inversa")
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
