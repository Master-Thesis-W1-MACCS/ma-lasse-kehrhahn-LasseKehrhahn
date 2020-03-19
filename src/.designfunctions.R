.create_designmatrix <- function(X,Y,DENS,rowname="X",colname="Y") {
 #generating A_X_Y  => design matrix
  
  
repeat
  {

  if(DENS == 2) {                 #Density is defined in gen_EAD for each matrix separatly (e.g. DENS_CNFR), if density = 2 we want a diagonal matrix
    
    if (X!=Y) {A_XY = 'error'}    #Size must be identical nrow = ncol
    else {
        A_YX = diag(X)# full decoupled
      }
    
  }
  
  else if(DENS == -1) {           #if density = -1 we want a matrix with a random density between the desired boundaries (eg. [0.4,0.7])
    DENS_MIN = 0.4
    DENS_MAX = 0.7
    DENS = runif(1, DENS_MIN, DENS_MAX);
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),nrow=X,ncol=Y) ## 1/0 DENSITY 
  
  }
  
  else {                          #if density is set to a fixed value 
    
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),nrow=X,ncol=Y) ## 1/0 DENSITY 
  
  }

  ROW_ZEROS<-any(rowSums(A_YX[,])==0)   #every product need at least one resource
  COL_ZEROS<-any(colSums(A_YX[,])==0)   #every resource needs to be used at least once
  
  if(ROW_ZEROS==FALSE & COL_ZEROS==FALSE) {  break  }   
  }
  

  
rownames(A_YX) = c(paste0(rowname, 1:nrow(A_YX)))
colnames(A_YX) = c(paste0(colname, 1:ncol(A_YX)))

return(A_YX)
 
}






