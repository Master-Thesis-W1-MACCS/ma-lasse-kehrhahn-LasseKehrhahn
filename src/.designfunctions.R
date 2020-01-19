.create_designmatrix <- function(X,Y,DENS) {
 #generating A_X_Y  => design matrix
  
     #//  identity matrix
  if(DENS == 2) {
    
    if (X!=Y) {A_XY = 'error'}
    else {
        A_YX = diag(X)# full decoupled
      }
    
  }
  
  else if(DENS == -1) {
    DENS_MIN = 0.4
    DENS_MAX = 0.7
    DENS = runif(1, DENS_MIN, DENS_MAX);
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),Y,X) ## 1/0 DENSITY 
  
  }
  
  else {
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),Y,X) ## 1/0 DENSITY 
  }

  
A_YX = .checkzero(A_YX,DENS)

return(A_YX)
 
}


.checkzero <-function(Matrix,DENS) {
  
 # Matrix = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
 #  DENS = 2
  ## ===================== EXCPETION HANDLER ====================
  
  # EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
  # in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 
  
  repeat
  {
    ROW_ZEROS<-any(rowSums(Matrix[,])==0)   #every product need at least one resource
    COL_ZEROS<-any(colSums(Matrix[,])==0)   #every resource needs to be used at least once
    
    if(ROW_ZEROS==FALSE & COL_ZEROS==FALSE) {  break  }
    
    Matrix =.create_designmatrix(nrow(Matrix),ncol(Matrix),DENS)
 
    
  }
  return(Matrix)
}




