## Estas funciones almacenan en caché el inverso de una matriz.

#La primera función, makeCacheMatrix crea una "matriz" especial, 
#que es realmente una lista que contiene una función para:
#1.establecer el valor de la matriz
#2.obtener el valor de la matriz
#3.establecer los valores de la matriz inversa
#4.obtener los valores de la matriz inversa

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) a <<- solve
  getsolve <- function() a
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function

#La siguiente función calcula la matriz inversa de la "matriz" especial creada
#con la función anterior. Sin embargo, comprueba primero si la matriz inversa
#ya ha sido calculada. Si es así, obtiene la matriz inversa del caché y omite el cálculo. 
#De lo contrario, calcula la matriz inversa de los datos y establece los valores de la 
#matriz inversa en el caché a través de la función setsolve.

cacheSolve <- function(x, ...) {
  a <- x$getsolve()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setsolve(a)
  a
}
