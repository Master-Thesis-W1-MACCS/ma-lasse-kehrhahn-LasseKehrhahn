#Independent Axiom

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

# A_YX = matrix(c(1,0,1,1,1,1),byrow=TRUE,nrow=2)
# checkIndep(A_YX)

checkIndep <- function(matrix){
  
  rowSum = rowSums(matrix)
  colSum = colSums(matrix)
  columns = length(matrix[1,])
  check_array <- c()
  checked_rows <- c()
  min_index = 0
  
  #Aeussere Schleife laeuft so oft durch, wie Anzahl der Zeilen der Matrix
  for (i in 1:(length(rowSum))){
    min_value = columns+1                                         #Groesser als max. Anzahl der Spalten
    
    #Auswahl der Zeile mit der niedrigsten Anzahl von Eintraegen, die noch nicht bisher gecheckt wurde
    for (j in 1:(length(rowSum))){
      if (rowSum[j]<min_value && !is.element(j,checked_rows)){
        min_value = rowSum[j]
        min_index = j
      }
    }
    checked_rows = append(checked_rows,min_index)                 #array checked_rows speichert Index der Zeile
    
    #Spaltenindizes der Nicht-Null-Eintraege der Zeile werden in row_array gespeichert
    row_array <- c()
    for (k in 1:(length(matrix[min_index,]))){
      if (matrix[min_index,k] != 0){
        row_array = append(row_array,k)
      }
    }
    
    #Counter zaehlt, wie viele neue Spaltenindizes (inkl. zuvor gepruefter Zeilen) Nicht-Null-Eintraege in der aktuellen Zeile haben
    counter = 0
    new_array <- c()
    for (column in row_array){
      if(!is.element(column,check_array)){
        counter=counter+1
        new_array=append(new_array,column)                      #Neue Spaltenindizes werden in new_array gespeichert
      }
    }
    
    if (counter == 0){                                          #kein neuer Nicht-Null-Eintrag, abhaengiges Design
      return(paste("Coupling in FR", min_index))
      
    } else if (counter>1) {                                     #mehr als ein neuer Nicht-Null-Eintrag
        for (col in new_array){                                 #Pruefen, ob die neuen Nicht-Null-Entr?ge auch in anderen Zeilen der Matrix vorkommen
          if (colSum[col]>1){
            return(paste("CM", toString(new_array) ,"causes coupling in FR", min_index))                                #Gucken, dass es alle neuen Werte in keiner anderen Zeile gibt
          }
        }
    }
    check_array = append(check_array,new_array) 
  }
  #print(matrix)
  #print(design_check)
  
    return("decoupled")
}

# A_YX = matrix(c(1,1,0,1,1,1),byrow=TRUE,nrow=2)
# p=CM_prob_to_sat(A_YX)
# I_total=infoCont_vektor(p)


#Information Axiom

CM_prob_to_sat <- function(matrix){
  probVektor <- c()
  CM_prob_to_sat <- c()
  #Erstelle Wahrscheinlichkeitsmatrix 
  #Spaltensumme > 1 heißt mehrere Abhängigkeiten, daher erhöhte Komplexität.
  #Erhöhte Komplexität bedeutet geringere Wahrscheinlichkeit, dass die FR erfüllt werden können.
  
  colSum = colSums(matrix)
  CM_prob_to_sat <- 1/colSum
  
  return(CM_prob_to_sat)
}

infoCont_vektor <- function(vektor){
  #Ermittle Informationsgehalt pro CM_i in Array, bzw. Total = Summe(Array)
  #Eine geringere Erfolgswahrscheinlichkeit impliziert, dass mehr Informationen benötigt werden.
  #Der Informationsgehalt wird ermittelt: I = -log2(1/p_FR), p = Wahrscheinlichkeit

    for(j in 1:NROW(vektor)){
      if (vektor[j]!=0){
        vektor[j] = -log2(vektor[j])
      }
    }

  I_total = sum(vektor)
  return(I_total)
  
}   

convToProbMatrix <- function(matrix){
  
  #Erstelle Wahrscheinlichkeitsmatrix 
  #Spaltensumme > 1 heißt mehrere Abhängigkeiten, daher erhöhte Komplexität.
  #Erhöhte Komplexität bedeutet geringere Wahrscheinlichkeit, dass die FR erfüllt werden können.
 
   colSum = colSums(matrix)
  probMatrix <- sweep((matrix),2,colSum,"/")
  
  return(probMatrix)
}

infoCont <- function(matrix){
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

  I_CM = colSums(matrix)/NUMB_FR
  I_total = sum(I_CM)
  return(I_total)
  
}   
     #Das Erlangen von Informationen setzt immer bestimmte Prozesse voraus, was Kosten verursacht.
     #Gesamtkosten von I abhängig machen

calc_I_max <- function(matrix) {
  I_max = -ncol(matrix)*log2(1/nrow(matrix))
  
}