## I am  going to describe how to cache a mtrix inverse and reurn this cache 

## Cache the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {

        mat <- NULL
	        set <- function(y) {
		                x <<- (y)
				   mat <<- NULL
						        }

	 get <- function() x
	 setinv <- function(solve) mat <<- solve
	 getinv <- function() mat
		   list(set = set, get = get,setinv = setinv,getinv = getinv)

}


## Return the cached inverse matrix

cacheSolve <- function(x, ...) {

  mat <- x$getinv()
       if(!is.null(mat)) {
	                  message("getting cached inv matrix")
			    return(mat)
			          }	
				  data <- x$get()
				 mat <- solve(data, ...)
				  x$setinv(mat)
				  mat


       
}

## to apply the above functions

##x=matrix(c(1,3,5,3),nrow=2,ncol = 2)
##> x
##     [,1] [,2]
##[1,]    1    5
##[2,]    3    3
##> 

##m=makeCacheMatrix(x)
## cacheSolve(m)
##      [,1]        [,2]
##[1,] -0.25  0.41666667
##[2,]  0.25 -0.08333333
##> cacheSolve(m)
##getting cached inv matrix
##      [,1]        [,2]
##[1,] -0.25  0.41666667
##[2,]  0.25 -0.08333333
##> 
