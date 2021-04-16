##writing comments describing this functions

##there is a function called makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv

makeCacheMatrix = function(x = matrix) {
     inv=NULL
     set=function(y){
         x==y
         inv=NULL   ##setting inverse as null
     }
     get= function() {x}
     setInverse = function(inverse) {inv==inverse}
     getInverse = function() {inv}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve = function(x, ...) {
    inv=x$getInverse()
    if(!is.null(inv)) {
         message("getting cachedd data") ##this is used so that we can get the previous cached matrix
         return(inv)
    }
    mat=x$get()     ##this is used to get the matrix
    inv=solve(mat, ...)  ##this will help to get the inverse
    x$setInverse(inv)    ##this will help to set the inverse
    inv
}