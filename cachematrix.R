## The code for this assignment creates a matrix object and methods
##  to solve and cache its inverse to potentially save time
##
## The structure of the code is the same as the vector mean caching
##  example given for the assignment, but variable names were modified
##  in an attempt to make the operation more clear.
##
## Functionality was tested using the excellent examples provided by
##  Fu Sheng Wang in a discussion thread at:
##  https://class.coursera.org/rprog-002/forum/thread?thread_id=696


## makeCacheMatrix creates a matrix object with methods to set and get
##  the matrix and its solutions. Calling this function creates an object
##  with the input matrix, a default NULL solution, and a set of functions
##  bound to the object for manipulating it.

makeCacheMatrix <- function(stored.matrix = matrix()) {
    #default to no solution
    solution<-NULL
    #now define the methods for manipulating the object
    set <- function(input.matrix) {
        stored.matrix<<-input.matrix
        #if a new matrix has been set, reset the solution to NULL
        solution <<- NULL
    }
    get <- function() stored.matrix
    setsolution <- function(solve) solution <<- solve
    getsolution <- function() solution
    #return a list of the applicable methods
    list (set=set, get=get, setsolution=setsolution, getsolution=getsolution)
}


## CacheSolve accepts as input an object created by makeCacheMatrix
##  If the object has not already been solved, the function calculates it.
##  If the object has already been solved, the function returns that
##   solution and a message to the user that the cached value is being used.

cacheSolve <- function(stored.matrix, ...) {
        #first check for an existing cached solution
        solution<- stored.matrix$getsolution()
        #if the returned solution is not null, then return it
        if(!is.null(solution)) {
            message("getting cached data")
            return(solution)
        }
        #if the soltion *was* null, then calculate the inverse, store it, and
        # return it
        data <- stored.matrix$get()
        solution <- solve(data,...)
        stored.matrix$setsolution(solution)
        ## Return a matrix that is the inverse of 'x'
        solution
        
}
