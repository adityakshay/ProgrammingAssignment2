## Computation of inverse of a matrix might take up significant processing time.
## In scenarios where we are required to compute the inverse of a particular matrix
## iteratively, we can store the inverse matrix in cache and get the value from the
## cache in place of calculating it again. The below two functions have to be used
## in conjunction to pass the inverse of the matrix to cache when it is calculated
## for the first time. After that we can use the cacheSolve function to get the 
## inverse of the same matrix from the cache

## makeCacheMatrix function takes invertible matrix as the argument and returns
## a special list with the following four components:
## 1. setm(set matrix): Set the value of the matrix
## 2. getm(get matrix): Get the value of the matrix
## 3. setinv(set inverse matrix): Set the value of the inverse of the matrix
## 4. getinv(get inverse matrix): Get the value of the inverset of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        list(
                setm=function(y) {
                        x<<-y
                        inv<-NULL
                },
                getm=function() x,
                setinv=function(invmatrix) inv<<-invmatrix,
                getinv=function() inv
        )
}


## cacheSolve takes the special list returned from makeCacheMatrix funcion  as 
## argument (xlist) and uses the getinv furnction to check the value of the inv 
## (inverse matrix stored in cache). If it finds the value of the matrix in cache,
## it does not compute the inverse and returns value from cache. Else it gets the
## data useing getm function and computes inverse. The computed value is then 
## returned to cache using setinv function. The final output is the inverse of
## of the matrix(inv)

cacheSolve <- function(xlist, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- xlist$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- xlist$getm()
        inv <- solve(data, ...)
        xlist$setinv(inv)
        inv
}

## Example: mymatrix<-matrix(rnorm(1:16),4,4)
## mymatrixlist<-makeCacheMatrix(mymatrix)
## cacheSolve(mymatrixlist)
## subsequently cacheSolve(mymatrixlist) returns inv value stored in the cache
## and following message is displayed: getting cached data

