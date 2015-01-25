
makeCacheMatrix <- function(x=matrix()) {
    xinv <- NULL   # this is where the result of inversion is stored
    set <- function(y) {
	  x <<- y
	  xinv <<- NULL     # it also initialises m to null
      }

    get <- function() x      # return the input matrix
    setImatrix <- function(Imatrix) xinv <<- Imatrix    # set the inversed matrix
    getImatrix <- function() xinv   # return the inversed matrix

    list(set = set, get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}



cacheSolve <- function(x, ...) {
    m <- x$getImatrix()  # get the inversed matrix from object x
    if(!is.null(m)){     # if the inversion result is there
        message("Cached data found.")
        return(m)        # return the calculated inversion
    }
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$setImatrix(m) # assigns resulting inverse matrix to object x
        return(m)
    
}

