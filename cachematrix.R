
#we have define two function makeCachematrix and cacheSolve
#makeCachematrix consists of set, get, setInverse,getInverse
makeCachematrix<-function(x=matrix()){
inv<-NULL                        #initializing inverse as null
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function()x                 #function to get matrix
setInverse<-function(inverse)(inv <<- inverse)
getInverse<-function(){inv}          #function to get inverse of the matrix
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


cacheSolve<-function(x,...){
inv<-x$getInverse()
if(!is.null(inv)){             #to check whether inverse is null or not
message("getting cached data")
return(inv)                     #this returns inverse value
}
mat<-x$get()
inv<-solve(mat,...)            #calculates inverse value
x$setInverse(inv)
inv                             #returns the inverted matrix
}

#input
pmatrix<-makeCachematrix(matrix(1:4,nrow=2,ncol=2))
pmatrix$get()
pmatrix$getInverse()
cacheSolve(pmatrix)
pmatrix$getInverse()