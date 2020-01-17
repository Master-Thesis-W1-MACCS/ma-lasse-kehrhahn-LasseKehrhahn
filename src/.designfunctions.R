.create_designmatrix <- function(X,Y,DENS) {

  
    #generating A_X_Y  => design matrix 
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

  
return(A_YX)
 
}
