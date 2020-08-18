NUMB_CM = c(2,4,6,8,10) #different plots for each
NUMB_FR = c(2,4,6,8,10)
DENS_FRCM = c(0.2, 0.35, 0.5, 0.65, 0.8) #x-Achse
color = c("blue","green","red","yellow","black")
SIM_NUMB = 100

infoContArray = c()
infoContAv = c()
infoContSD = c()

for (i_DENS_FRCM in seq_along(DENS_FRCM)) {
  
  for (i_NUMB_CM in seq_along(NUMB_CM)) {
    ACTUAL_DENS = DENS_FRCM[i_DENS_FRCM]
    
    for (j in 1:SIM_NUMB) {
      A_FRCM = .create_designmatrix(NUMB_FR[i_NUMB_CM], NUMB_CM[i_NUMB_CM], ACTUAL_DENS, "FR", "CM")
      infoContArray[j] = infoCont(convToInfoMatrix(A_FRCM),NUMB_FR[i_NUMB_CM])
    }
    infoContAv[i_NUMB_CM] = mean(infoContArray)
    infoContSD[i_NUMB_CM] = sd(infoContArray)
  }
  
  if (i_DENS_FRCM==1){
    plot(NUMB_CM,infoContAv,ylim=c(0,30), type="o", main="Total Information Content",col=color[i_DENS_FRCM]) #infoContAv[length(NUMB_CM)]+infoContSD[length(NUMB_CM)]+1
    lines(rbind(NUMB_CM,NUMB_CM,NA),rbind(infoContAv-infoContSD,infoContAv+infoContSD,NA),col=color[i_DENS_FRCM])
  } else{
    points(NUMB_CM,infoContAv)
    lines(NUMB_CM,infoContAv,col=color[i_DENS_FRCM])
    lines(rbind(NUMB_CM,NUMB_CM,NA),rbind(infoContAv-infoContSD,infoContAv+infoContSD,NA),col=color[i_DENS_FRCM])
  }
}
legend(x="topleft",c("0.2","0.35","0.5","0.65","0.8"),col=color,lty=c(1,1,1,1,1))

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

convToProbMatrix <- function(matrix){
  
  #Erstelle Wahrscheinlichkeitsmatrix 
  #Spaltensumme > 1 heißt mehrere Abhängigkeiten, daher erhöhte Komplexität.
  #Erhöhte Komplexität bedeutet geringere Wahrscheinlichkeit, dass die FR erfüllt werden können.
  
  colSum = colSums(matrix)
  probMatrix <- sweep((matrix),2,colSum,"/")
  
  return(probMatrix)
}

infoCont <- function(matrix,NUMB_FR_current){
  #Ermittle Informationsgehalt pro CM_i in Array, bzw. Total = Summe(Array)
  #Eine geringere Erfolgswahrscheinlichkeit impliziert, dass mehr Informationen benötigt werden.
  #Der Informationsgehalt wird ermittelt: I = -log(1/p_FR), p = Wahrscheinlichkeit
  
  
  for (i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      if (matrix[i,j]!=0){
        matrix[i,j] = -log(matrix[i,j],2)
      }
    }
  }
  
  I_CM = colSums(matrix)/NUMB_FR_current
  I_total = sum(I_CM)
  return(I_total)
  
}   