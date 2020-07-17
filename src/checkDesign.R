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


calcInfoCont <- function(matrix, DENS_FRCM_measured){
  
     #Kalkuliere Spalten-Summen
     #Summe > 1 heißt mehrere Abhängigkeiten, daher erhöhte Komplexität
     #Erhöhte Komplexität bedeutet geringere Wahrscheinlichkeit, dass die FR erfüllt werden können.
     #Eine geringere Erfolgswahrscheinlichkeit impliziert, dass mehr Informationen benötigt werden.
     #Zusammenhang zwischen Spaltensummen und p herstellen. p = Wahrscheinlichkeit, dass FR erfüllt wird. ZB.:
         #p=1/(Anz.SpaltenSummen > 1/2... = benötigen komplexe Komponenten)
         #Komponenten-Komplexität ist Abhängig von Spaltensumme.
         #Je größer die Spaltensumme, desto Komplexer, zB.: p_CM_i = 1/Spaltensumme --> Bilde Vektor p_CM = c()
         #Ermittle Wahrscheinlichkeit, dass das FR erfüllt wird.
         #p_FR_i = FR_i Zeile*p_CM = c()
         #p_FR_i = avarage(p_FR_i) --> Wahrscheinlichkeit in %
         #bilde p_FR = c()
     #Der Informationsgehalt wird ermittelt: I = -log(1/p_FR) = c() 
  
  
}   
     #Das Erlangen von Informationen setzt immer bestimmte Prozesse voraus, was Kosten verursacht.
     #Gesamtkosten von I abhängig machen