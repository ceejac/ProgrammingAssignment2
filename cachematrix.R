##  functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( m = matrix() )
 
 {
	inv<-NULL
        
    ##set matrix
        
        set <- function(mat) 
 	{
		m <<- mat
		inv <<- NULL
	} 
    
    ##get matrix
        
        get <- function() m

    ##set inverse
        
        setinverse <- function(inverse) inv<<-inverse

    ##get inverse
 	
 	getinverse <- function() inv
    
    ##list of the methods
	
	list(set = set, get = get,
	     setinverse = setinverse,
             getinverse = getinverse)

 }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
 
 {
    
    ## Return a matrix that is the inverse of 'x'
	
	m<- x$getinverse()
	
    ## Just return the inverse if its already set	
       
       	if(!is.null(m)) 
         {
                message("getting cached data")
                return(m)
         }
        
        
    ## Get the matrix
	
	data <- x$get()
    
    ## Calculate the inverse 
	
	m <- solve(data)
   
    ## Set the inverse to the object
	
	x$setinverse(m)
    
    ## Return the matrix
        
        m

 }
