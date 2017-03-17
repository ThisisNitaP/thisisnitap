
  ## Second assignment in coursera R Programming to write an R function
  ## that is able to cache inverse of matrix.

  ## This function "makeCacheMatrix" creates a special "matrix" object that can 
  ## cache its inverse.
  makeCacheMatrix<- function(x=matrix()){
  ## Initializing inverse of matrix in variable with "NULL"
                  matrixInverse<-NULL
  ## defining set function to demonstrate lexical scoping 
                   set<-function(y){
  ## Assigning x value of y
                           x<<-y
  ## Assigning Matrixinverse as NULL                           
                           matrixInverse<<- NULL
                   } ## closure of set function 
  ## Get function definition, retriving value of matrix 
                   get<- function()x
  ## Defining logic of cache in matrix setMatrixinverse
  ## Assigning variable matrixInverse the value passed in the function                   
                  setMatrixInverse<- function(inverse) matrixInverse<<- inverse
  ## function definition for reading and retrieving value of inverse which is stored in setMatrixInverse                  
                  getMatrixInverse<-function() matrixInverse
  ## storing the values in a list 
                  list(set=set, 
                       get=get, setMatrixInverse= setMatrixInverse,
                       getMatrixInverse=getMatrixInverse)

  } ## end of makecacheMatrix


  ## This function computes the inverse of the special "matrix" returned by
  ## makeCacheMatrix above. If the inverse has already been calculated
  ## (and the matrix has not changed), then cacheSolve should retrieve the inverse
  ## from the cache.

  cacheSolve <- function(x, ...) {
  ## Retrieving value from getMatrixInverse and assigning to matrixInverse          
          matrixInverse <- x$getMatrixInverse()
  ## logic to see if matrixInverse has any cached data          
          if(!is.null(matrixInverse)) {
  ## printing if cached value available              
                  message("getting cached data:")
                  printCoefmat(matrixInverse)
                  return(matrixInverse)
          }
  ## logic to calculate inverse matrix
          data <- x$get()
  ## calculated matrix inverse ad assigning it to matrix inverse         
          matrixInverse <- solve(data, ...)
  ## printing matrix inverse          
          message("Cached data not available, getting o/p from computtion:")
          printCoefmat(matrixInverse)
  ## storing value in setmetrixInverse tp cache value          
          x$setMatrixInverse(matrixInverse)
  } ## end of function CacheSolve

  ## Test Data for testing the above function
  test<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
  test
  test1<-makeCacheMatrix(test)
  test1
  test2<-cacheSolve(test1)
  test2

