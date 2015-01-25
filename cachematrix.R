#These pair of functions save and retreve data from an environment below the "global" workspace. The data is saved as 'm'. 
#The first function generates the values, while the second funuction does the computing.
#Start by feeding makeCacheMatrix an invetabl matrix. You will have to define a name for this new, inverted matrix.
	#If the name has already been used, the old data will be overwriten.
	#If there is a new name, both the old and new data will be preserved in different 'environments'.
#Second, pass the same name through 'cacheSolve'. If the matrix has been inverted before, cached data will be presented.
#Otherwise, the inverse will be computed on the spot.

makeCacheMatrix<- function(MX = matrix()) {  
    #first we set m to be null essentially resetting the process
    m <- NULL    
    #this function is used in future cases to avoid having to reassign the function list to another variable.
    set <- function (y) {
          MX <<- y
          m <<- NULL
    }
    
    #This returns the entry matrix 
    get <- function() MX
    
    #This assigns the argument to m to a new environment accessable after this function is finished. 
    setinv <- function(inv) m <<- inv
    
    #Returns the value for m 
    getinv <- function () m
    
    #Optional: returns each of the functions along with location where data is saved.
    list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

#this defines a new function which is applied to the function.
cacheSolve <- function(makeCacheMatrix) {
    #'m' is pulled from the function above. If this is the first time this matrix has been run, it will be 'NULL'. 
    m <- makeCacheMatrix$getinv()
    
    #If m is NOT null, then return the inverted matrix stored in memory.
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    
    #Otherwise, pull data from the first function in order to calculate a inverse 
    MX2 <- makeCacheMatrix$get()
    
    #Calculate a inverse based on the data
    m <- solve(MX2)
    
    #Now set this new value to memory.
    makeCacheMatrix$setinv(m)
    
    #Lastly output the value of m
    m
}