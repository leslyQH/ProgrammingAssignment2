
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix<- NULL  # creación de variable para guardar la inversa 
    # es nulo porque no se ha calculado nada aun
    
    
    # Cambia la matriz guardada
    set <- function(y) {
        x <<- y #se asigna a x el valor de y, pero en el entorno padre (global)
        inverse_matrix <<- NULL # al cambiar la matriz, invalida la cacheada
    }
    
    # devuelve la matriz actual
    get <- function() x
    
    # Función interna que que guarda en caché la inversa calculada, 
    # usando <<- (para que persista en el objeto).
    setinverse <- function(inv){ 
        inverse_matrix <<- inv
    }
    
    # Función interna que devuelve lo que haya en inverse_matrix
    getinverse <- function() inverse_matrix
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## Computes or retrieves the cached inverse of a matrix.'

cacheSolve <- function(x, ...) {
    # Recibe el objeto creado por makeCacheMatrix.
    # ... permite pasar argumentos extra a solve() 
    #    inv<- x$set(1)
    inv <- x$getinverse()
    # Intenta leer la inversa cacheada. Si ya se calculó antes 
    # y la matriz no ha cambiado, aquí no es NULL.
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    
    x$setinverse(inv)
    inv
}
