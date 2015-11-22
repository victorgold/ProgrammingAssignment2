##Use these funcions to calculate a inverse matrix and store a memory cache.  

#this function provides a special matrix to manipulated cached inverse
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix = NULL #
        ## Matrix setter
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        } 
        ## Matrix getter
        get = function() x
        ##Inverse matrix setter
        setinv = function(inverse) inverseMatrix <<- inverse 
        ##Inverse matrix getter
        getinv = function() inverseMatrix
        list(
        		set=set, 
			get=get,
        		setinv=setinv,
        		getinv=getinv
        	)
}

#this function cumputes a inverse matrix and provides a facede to access a cache
cacheSolve <- function(x, ...) {
        inverseMatrix = x$getinv()
        
        # if the inverse was cached
        if (!is.null(inverseMatrix)){
                # get cache. 
                message("getting cached data")
                return(inverseMatrix)
        }
        #calculates the inverse and store in moemory cache
        mat.data = x$get()
        inverseMatrix = solve(mat.data, ...)
        x$setinv(inverseMatrix)
        return(inverseMatrix)
}
