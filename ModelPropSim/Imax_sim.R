NUMB_CM = c(2,4,6,8,50)
NUMB_FR = c(2,4,6,8,50)
DENS_FRCM = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8) #x-Achse des Plots
color = c("blue","green","red","yellow","black")
SIM_NUMB = 10000

infoContArray = c()
infoContAv = c()
infoContSD = c()

for (i_NUMB_CM in seq_along(NUMB_CM)) {
  
  for (i_DENS_FRCM in seq_along(DENS_FRCM)) {
    ACTUAL_DENS = DENS_FRCM[i_DENS_FRCM]
    
    for (j in 1:SIM_NUMB) {
      A_FRCM = .create_designmatrix(NUMB_FR[i_NUMB_CM], NUMB_CM[i_NUMB_CM], ACTUAL_DENS, "FR", "CM")
      infoContArray[j] = 100*infoCont(CM_prob_to_sat(A_FRCM))/calc_I_max(A_FRCM)
    }
    infoContAv[i_DENS_FRCM] = mean(infoContArray)
    infoContSD[i_DENS_FRCM] = sd(infoContArray)
  }
  
  if (i_NUMB_CM==1){
    plot(DENS_FRCM,infoContAv,ylim=c(0,100), type="o", main="Total Information Content",col=color[i_NUMB_CM]) #infoContAv[length(NUMB_CM)]+infoContSD[length(NUMB_CM)]+1
    lines(rbind(DENS_FRCM,DENS_FRCM,NA),rbind(infoContAv-infoContSD,infoContAv+infoContSD,NA),col=color[i_NUMB_CM])
  } else{
    points(DENS_FRCM,infoContAv)
    lines(DENS_FRCM,infoContAv,col=color[i_NUMB_CM])
    lines(rbind(DENS_FRCM,DENS_FRCM,NA),rbind(infoContAv-infoContSD,infoContAv+infoContSD,NA),col=color[i_NUMB_CM])
  }
  
  #plot(DENS_FRCM,infoContAv,ylim=c(0,infoContAv[length(DENS_FRCM)]+infoContSD[length(DENS_FRCM)]+1), add=TRUE)
  #lines(rbind(DENS_FRCM,DENS_FRCM,NA),rbind(infoContAv-infoContSD,infoContAv+infoContSD,NA))
}
legend(x="topleft",c("2x2","4x4","6x6","8x8","10x10"),col=color,lty=c(1,1,1,1,1))

calc_I_max <- function(matrix) {
  I_max = -ncol(matrix)*log2(1/nrow(matrix))

}

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

infoCont <- function(vektor){
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