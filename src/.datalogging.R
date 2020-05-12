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
  EAD$CC[PRODUCT] = EAD$CC #total cost of the product
  o[PRODUCT] = o #run, repetition
  nn[PRODUCT] = nn #which kind of design? 
  
  DATApre = data.frame(o,nn,PRODUCT,EAD$CC) # construct the dataframe 
  
  colnames(DATApre) = c('o','nn','PRODUCT','CC')
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}

