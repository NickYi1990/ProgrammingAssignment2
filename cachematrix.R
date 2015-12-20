

# This function return a list that contains four function objects,they and their function are listed below:
# SetCachedMatrix(matrix_in)          Set Cached matrix
# GetCachedMatrix                     Get Cached matrix
# SetCachedMatrixInv(matrix_in_inv)   Set Cached Inverse matrix
# GetCachedMatrixInv                  Get Cached Inverse matrix
makeCacheMatrix <- function(CachedMatrix = matrix()) {#initialize CachedMatrix to matrix()
      #initialize CachedMatrixInv matrix to matrix()
      CachedMatrixInv <- NULL
      
      SetCachedMatrix <- function(matrix_in){
        if(identical(matrix_in, CachedMatrix)){
          print("Input matrix is identical with last time, so we do not initialize the Inv matrix and do not change the CachedMatrix")
        }
          else{
            CachedMatrix <<- matrix_in
            #If we change the input matrix, then we set the Inv matrix to zero.
            CachedMatrixInv <<- NULL
          }
      }
      
      GetCachedMatrix <- function(){
        CachedMatrix
      }
      
      SetCachedMatrixInv <- function(matrix_in_inv){
        CachedMatrixInv <<- matrix_in_inv
      }
      
      GetCachedMatrixInv <- function(){
        CachedMatrixInv
      }
      
      list(SetCachedMatrix = SetCachedMatrix, 
           GetCachedMatrix = GetCachedMatrix,
           SetCachedMatrixInv = SetCachedMatrixInv,
           GetCachedMatrixInv = GetCachedMatrixInv)
}



# This function is used to compute new inv matrix if needed
cacheSolve <- function(x=makeCacheMatrix(), ...) {
      ## Return a matrix that is the inverse of 'x'
      MatrixInv <- x$GetCachedMatrixInv()
      
      if(!is.null(MatrixInv)){
        return(MatrixInv)
      }
      
      #If inv matrix is null then get the cached matrix and compute the new inv matrix. 
      data <- x$GetCachedMatrix()
      x$SetCachedMatrixInv(solve(data))
      
      #return the inv matrix
      solve(data)
}
