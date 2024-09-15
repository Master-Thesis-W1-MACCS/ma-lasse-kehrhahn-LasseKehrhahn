#1. Parameter Eingabe
#2. Führe checkDesign.R und .designfunctions.R aus und misst tatsächliche DENS, 
#3. Speichere Ergebnisse der Simulationsdurchläufe
#4. Variiere Parameter, aber speichere Zwischenergebnisse (Simulationsdurchgänge)
#5. Erstelle Graphen: Ergebnisse(Parametervariation)
#6. Pro Parametervariation(i) xxx Simulationsdurchgänge. Ergebnis ist dann die Menge wie oft TRUE rauskommt.
#   Beispiel:
#   Simulationen pro Variation: 100, DENS_FRCM=0.4, Matrix: 3x3, TRUE:30, FALSE:70, Diagrammeintrag: x: 0.4 y: 30/100=30%

NUMB_CM = c(2,4,6,8,10)
NUMB_FR = c(2,4,6,8,10)
DENS_FRCM = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8)
color = c("blue","green","red","yellow","black")
SIM_NUMB = 1
PERCENTAGE = c()

for (i_NUMB_CM in seq_along(NUMB_CM)){

  for (i_DENS_FRCM in seq_along(DENS_FRCM)) {
 
     ACTUAL_DENS = DENS_FRCM[i_DENS_FRCM]
     counter = 0
   
     for (j in 1:SIM_NUMB) {
       A_FRCM = .create_designmatrix(NUMB_FR[i_NUMB_CM],NUMB_CM[i_NUMB_CM],ACTUAL_DENS,"FR","CM")
    
       if (checkDesign(A_FRCM)) {
         counter=counter+1
       }
     }
     PERCENTAGE[i_DENS_FRCM] = (counter/SIM_NUMB)*100
  }
  
  if (i_NUMB_CM==1){
    plot(DENS_FRCM,PERCENTAGE,ylim=c(0,100), type="o", main="Indep. Chance",col=color[i_NUMB_CM]) #infoContAv[length(NUMB_CM)]+infoContSD[length(NUMB_CM)]+1
  } else{
    points(DENS_FRCM,PERCENTAGE)
    lines(DENS_FRCM,PERCENTAGE,col=color[i_NUMB_CM])
  }
}
legend(x="topright",c("2x2","4x4","6x6","8x8","10x10"),col=color,lty=c(1,1,1,1,1))

checkDesign <- function(matrix){
  
  design_check = TRUE
  rowSum = rowSums(matrix)
  colSum = colSums(matrix)
  columns = length(matrix[1,])
  check_array <- c()
  checked_rows <- c()
  min_index = 0
  
  #?u?ere Schleife l?uft so oft durch, wie Anzahl der Zeilen der Matrix
  for (i in 1:(length(rowSum))){
    min_value = columns+1                                         #Gr??er als max. Anzahl der Spalten
    
    #Auswahl der Zeile mit der niedrigsten Anzahl von Eintr?gen, die noch nicht bisher gecheckt wurde
    for (j in 1:(length(rowSum))){
      if (rowSum[j]<min_value && !is.element(j,checked_rows)){
        min_value = rowSum[j]
        min_index = j
      }
    }
    checked_rows = append(checked_rows,min_index)                 #array checked_rows speichert Index der Zeile
    
    #Spaltenindizes der Nicht-Null-Eintr?ge der Zeile werden in row_array gespeichert
    row_array <- c()
    for (k in 1:(length(matrix[min_index,]))){
      if (matrix[min_index,k] != 0){
        row_array = append(row_array,k)
      }
    }
    
    #Counter z?hlt, wie viele neue Spaltenindizes (inkl. zuvor gepr?fter Zeilen) Nicht-Null-Eintr?ge in der aktuellen Zeile haben
    counter = 0
    new_array <- c()
    for (column in row_array){
      if(!is.element(column,check_array)){
        counter=counter+1
        new_array=append(new_array,column)                      #Neue Spaltenindizes werden in new_array gespeichert
      }
    }
    
    if (counter == 0){                                          #kein neuer Nicht-Null-Eintrag, abh?ngiges Design
      design_check = FALSE
    } else if (counter>1) {                                     #mehr als ein neuer Nicht-Null-Eintrag
      for (col in new_array){                                 #Pr?fen, ob die neuen Nicht-Null-Entr?ge auch in anderen Zeilen der Matrix vorkommen
        if (colSum[col]>1){
          design_check = FALSE                                #Gucken, dass es alle neuen Werte in keiner anderen Zeile gibt
        }
      }
    }
    check_array = append(check_array,new_array) 
  }
  #print(matrix)
  #print(design_check)
  
  return(design_check)
}

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

# A_YX = matrix(c(0,2,5,14,0,0),byrow=TRUE,nrow=2)
# A_YX_x = conv_res_to_dep(A_YX)
# print(A_YX_x)


conv_res_to_dep <- function(matrix) {
  
  for (i in 1:nrow(matrix)){
     for(j in 1:ncol(matrix)){
       if (matrix[i,j]!=0){
          matrix[i,j] = 1
       }
     }
  }

    return(matrix)
}
































