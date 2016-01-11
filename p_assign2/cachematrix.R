
#First I create an empty matrix
makeCacheMatrix <- function(x = matrix()) {
        #within the function, I assign cache to NULL
        cache <- NULL
        #In setMatrix, the "<<-" operators allow x and cache to be
        #accessible from the cachesolve function; if you call
        #x$set() the matrix value will be reset, cache will again be 
        #NULL
        setMatrix <- function(y) {
                x <<- y
                cache <<- NULL
        } 
        getmatrix <- function() x
        cacheinverse <- function(solve)cache <<- solve
        getinverse <- function()cache
        #Below are the list of functions created when you initialize
        #makeCacheMatrix
        list(setMatrix = setMatrix, getmatrix = getmatrix,
             cacheinverse = cacheinverse, 
             getinverse = getinverse)   
}

  

cacheSolve <- function(x, ...) {
        #The cache "gets" the value of the matrix invers from makeCacheMatrix
        cache <- x$getinverse()
        #if the matrix inverse is present, you get the message,
        #if not, a matrix inverse is create with solve()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        #data is assigned to the value of getmatrix
        data <- x$getmatrix()
        #cache is assigned to the inverted matrix
        cache <- solve(data, ...)
        x$cacheinverse(cache)
        #the inverted matrix is returned with by the last function value
        cache
}
