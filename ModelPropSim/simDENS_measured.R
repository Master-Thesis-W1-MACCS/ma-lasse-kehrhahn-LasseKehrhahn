NUMB_CM = 10
NUMB_FR = 10
DENS_FRCM = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8)
SIM_NUMB = 1000
measureArray = c()
DENS_measured_avarage = c()
DENS_measured_SD = c()

for (i_DENS_FRCM in seq_along(DENS_FRCM)) {
  
  ACTUAL_DENS = DENS_FRCM[i_DENS_FRCM]
  
  for (j in 1:SIM_NUMB) {
    A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,ACTUAL_DENS,"FR","CM")
    measureArray[j] = count_nonzeros(A_FRCM) 
  }
  DENS_measured_avarage[i_DENS_FRCM] = mean(measureArray)
  DENS_measured_SD[i_DENS_FRCM] = sd(measureArray)
}

covData <- data.frame(DENS_FRCM,DENS_measured_avarage,DENS_measured_SD)
plot(DENS_FRCM,DENS_measured_avarage,ylim=c(0,DENS_measured_avarage[length(DENS_FRCM)]+DENS_measured_SD[length(DENS_FRCM)]+0.1))
lines(rbind(DENS_FRCM,DENS_FRCM,NA),rbind(covData$DENS_measured_avarage-covData$DENS_measured_SD,covData$DENS_measured_avarage+covData$DENS_measured_SD,NA))
rbind(covData$DENS_measured_avarage-covData$DENS_measured_SD,covData$DENS_measured_avarage+covData$DENS_measured_SD,NA)


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
    
    else if(DENS == 3) {                 #enter specific matrix
      
      A_YX = matrix(c(0,1,1,1,0,0),byrow=TRUE,nrow=2)
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

count_nonzeros <-function(your.matrix){
  colsum_nonzeros = colSums(your.matrix != 0)
  percentageofnonzeros = sum(colsum_nonzeros)/(ncol(your.matrix)*nrow(your.matrix))
  return(percentageofnonzeros)
}
