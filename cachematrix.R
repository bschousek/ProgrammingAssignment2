## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix and returns a 

makeCacheMatrix <- function(stored.matrix = matrix()) {
    solution<-NULL
    set <- function(input.matrix) {
        stored.matrix<<-input.matrix
        solution <<- NULL
    }
    get <- function() stored.matrix
    setsolution <- function(solve) solution <<- solve
    getsolution <- function() solution
    list (set=set, get=get, setsolution=setsolution, getsolution=getsolution)
}


## Write a short comment describing this function

cacheSolve <- function(stored.matrix, ...) {
        solution<- stored.matrix$getsolution()
        if(!is.null(solution)) {
            message("getting cached data")
            return(solution)
        }
        data <- stored.matrix$get()
        solution <- solve(data,...)
        stored.matrix$setsolution(solution)
        solution
        ## Return a matrix that is the inverse of 'x'
}
