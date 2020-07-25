################################################
# TRACKS THE PRODUCT LEVEL
################################################

.system_datalogging<-function(o,nn,EAD,DATA){
 # browser()
  
  

DATApre = data.frame(o,nn,EAD$NUMB_C, EAD$NUMB_CN, EAD$NUMB_FR, EAD$NUMB_CM, EAD$NUMB_PV, EAD$NUMB_RC,
                       EAD$DENS_CCN, EAD$DENS_CNFR, EAD$DENS_FRCM, EAD$DENS_CMPV, EAD$DENS_PVRC,
                       EAD$Q_VAR, EAD$RCC_VAR, EAD$DENS_FRCM_measured,
                       EAD$Diff_total)
  
colnames(DATApre) = c('o','nn','NUMB_C','NUMB_CN', "NUMB_FR", "NUMB_CM", "NUMB_PV", "NUMB_RC",
                     "DENS_CCN", "DENS_CNFR","DENS_FRCM","DENS_CMPV","DENS_PVRC",
                     "Q_VAR","RCC_VAR", "DENS_FRCM_m",
                     "DIFF_COST")    
 
DATA = rbind(DATA,DATApre) #put it together
 

  return(DATA)
}


.product_datalogging<-function(o,nn,EAD,DATAp){
 
  
  ####### DOES NOT WORK YET ############
  
  
  PRODUCT <- c(1:EAD$NUMB_C) #How many products per run 
  EAD$C_DEMAND[PRODUCT] = EAD$C_DEMAND #total cost of the product
  
  EAD$DENS_FRCM_measured[PRODUCT] =  round(EAD$DENS_FRCM_measured, digits = 2)
  EAD$INDEP_A_FRCM[PRODUCT] = EAD$INDEP_A_FRCM
  EAD$PVC[PRODUCT] = ceiling(EAD$PVC)
  EAD$CMC[PRODUCT] = ceiling(EAD$CMC)
  EAD$FRC[PRODUCT] = ceiling(EAD$FRC)
  EAD$CNC[PRODUCT] = ceiling(EAD$CNC)
  EAD$CCx[PRODUCT] = ceiling(EAD$CCx)
  EAD$CC[PRODUCT] = ceiling(EAD$CC) #total cost of the product, rounded for easier formatting in excel
  nn[PRODUCT] = nn #which kind of design? 
  
  DATApre = data.frame(nn,PRODUCT, EAD$C_DEMAND[PRODUCT], EAD$PVC[PRODUCT], EAD$CMC[PRODUCT], EAD$FRC[PRODUCT], EAD$CNC[PRODUCT], EAD$CCx[PRODUCT], EAD$CC[PRODUCT], EAD$INDEP_A_FRCM[PRODUCT], EAD$DENS_FRCM_measured[PRODUCT]) # construct the dataframe 
  
  colnames(DATApre) = c('nn','Customer', 'DEMAND', 'PVC', 'CMC', 'FRC', 'CNC', 'CCx', 'CC', 'INDEP', 'DENS_MEASURED')
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}

